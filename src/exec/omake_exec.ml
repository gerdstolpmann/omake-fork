(*
 * Execution service.  This is the wrapper around the remote
 * and local servers.
 *)


open Lm_printf
open Lm_thread_pool

open Omake_node


open Omake_exec_util
open Omake_exec_type
open Omake_exec_print
open Omake_exec_remote
open Omake_exec_notify
open Omake_options


module Exec =
struct
   (*
    * Local and remote servers.
    *)
   type ('venv, 'exp, 'value) server_handle =
      LocalServer  of ('venv, 'exp, 'value) Omake_exec_local.t
    | RemoteServer of ('venv, 'exp, 'value) Omake_exec_remote.t
    | NotifyServer of ('venv, 'exp, 'value) Notify.t

   (*
    * Information about the local server
    *    server_count   : number of jobs that can be run on this server
    *    server_handle  : handle for the actual server
    *    server_running : number of jobs that are actually running
    *    server_enabled : is server connected and ready?
    *)
   type ('venv, 'exp, 'value) server_info =
      { server_host            : string;
        server_count           : int;
        server_handle          : ('venv, 'exp, 'value) server_handle;
        mutable server_running : int;
        mutable server_enabled : bool
      }

   (*
    * The state:
    *    server_root    : location of the project root
    *    server_local   : local execution server
    *    server_servers : all the servers
    *
    *    Invariant: server_jobs and server_fds are equivalent
    *       server_jobs    : used with threads, a list of the currently active threads
    *       server_fds     : used with threads, a list of the currently active files
    *)
   type ('venv, 'exp, 'value) t =
      { server_root              : Dir.t;
        server_local             : ('venv, 'exp, 'value) Omake_exec_local.t;
        server_notify            : ('venv, 'exp, 'value) Notify.t;
        mutable server_servers   : ('venv, 'exp, 'value) server_info list;
        mutable server_fd_table  : ('venv, 'exp, 'value) server_info FdTable.t;
        mutable server_pid_table : Unix.file_descr IntTable.t
      }

   (*
    * Start a remote server.
    *)
   let start_local local options =
      { server_host    = "localhost";
        server_count   = opt_job_count options;
        server_running = 0;
        server_enabled = true;
        server_handle  = LocalServer local
      }

   let start_notify notify _options =
      { server_host    = "notify";
        server_count   = 0;
        server_running = 0;
        server_enabled = true;
        server_handle  = NotifyServer notify
      }

   let start_remote _root (machine, count) =
      { server_host    = machine;
        server_count   = count;
        server_running = 0;
        server_enabled = false;
        server_handle  = RemoteServer (Omake_exec_remote.create machine)
      }

   let create root options =
      let local = Omake_exec_local.create "local" in
      let notify = Notify.create "notify" in
      let servers =
         start_local local options
         :: start_notify notify options
         :: List.map (start_remote root) (opt_remote_servers options)
      in
         { server_root = root;
           server_local = local;
           server_notify = notify;
           server_servers = servers;
           server_fd_table = FdTable.empty;
           server_pid_table = IntTable.empty
         }

   (*
    * When the server is closed, kill all the jobs.
    *)
   let close server =
      List.iter (fun { server_handle = handle ; _} ->
            match handle with
               LocalServer local ->
                  Omake_exec_local.close local
             | RemoteServer remote ->
                  Omake_exec_remote.close remote
             | NotifyServer notify ->
                  Notify.close notify) server.server_servers

   (*
    * Print the status.
    *)
   let print_status tee options shell remote name _ =
      let remote =
         match remote with
            { server_handle = LocalServer _ ; _}
          | { server_handle = NotifyServer _ ; _} ->
               None
          | { server_host = host; server_handle = RemoteServer _ ; _} ->
               Some host
      in
         print_status tee options shell remote name

   (*
    * Find the best server.
    *)
   let find_best_server server =
      if !debug_remote then
         List.iter (fun { server_host = host;
                          server_count = count;
                          server_running = running;
                          server_enabled = enabled;
                          _
                        } ->
               eprintf "*** searching %s, count=%d, running=%d, enabled=%b@." host count running enabled) server.server_servers;
      let rec search best servers =
         match servers with
            server :: servers ->
               let { server_enabled = enabled;
                     server_count = count;
                     server_running = running;
                     _
                   } = server
               in
               let best =
                  if enabled && running <> count then
                     match best with
                        Some { server_running = running' ; _} ->
                           if running < running' then
                              Some server
                           else
                              best
                      | None ->
                           Some server
                  else
                     best
               in
                  search best servers
          | [] ->
               best
      in
         match search None server.server_servers with
            Some server ->
               server
          | None ->
               raise (Invalid_argument "Omake_exec.find_best_server: all servers are disabled")

   (*
    * Start a job.
    *)
   let spawn server_main shell options handle_sys_out handle_out handle_err name target commands =
      (* Start the job *)
      let id = Omake_exec_id.create () in
      let server = find_best_server server_main in
      let { server_running = running;
            server_handle = handle;
            _
          } = server
      in

      (* Handle a status message *)
      let handle_status = print_status (handle_sys_out id) options shell server name
      and direct_output = Omake_options.(opt_output options OutputDirect) in

      (* Start the job *)
      let status =
         match handle with
            LocalServer local ->
              Omake_exec_local.spawn direct_output local  shell id handle_out handle_err handle_status target commands
          | RemoteServer remote ->
               Omake_exec_remote.spawn false remote shell id handle_out handle_err handle_status target commands
          | NotifyServer notify ->
               Notify.spawn false notify shell id handle_out handle_err handle_status target commands
      in
      let () =
         match status with
            ProcessStarted _ ->
               server.server_running <- succ running
          | ProcessFailed ->
               ()
      in
         status

   let acknowledge_eof server_info options fd =
     match server_info.server_handle with
       | LocalServer local ->
           Omake_exec_local.acknowledge_eof local options fd
       | RemoteServer _ ->
           ()
       | NotifyServer _ ->
           ()


   let handle_eof server_info options fd =
     match server_info.server_handle with
       | LocalServer local ->
          let direct_output = Omake_options.(opt_output options OutputDirect) in
           Omake_exec_local.handle_eof direct_output local options fd
       | RemoteServer _ ->
           ()
       | NotifyServer _ ->
           ()


   let likely_blocking server_main =
     (* whether it is likely that we'll block the next time we wait *)
     List.exists
       (fun server_info ->
          match server_info.server_handle with
            | LocalServer local -> Omake_exec_local.likely_blocking local
            | RemoteServer _ -> true
            | NotifyServer _ -> false
       )
       server_main.server_servers

   (*
    * Select-based waiting.
    * Wait for input on one of the servers.
    *)
   let wait_select server options =
      let { server_notify = _notify;
            server_servers = servers;
            _
          } = server
      in
      let fd_table =
         List.fold_left
           (fun fd_table server ->
              let fd_set =
                match server.server_handle with
                  | LocalServer local ->
                      Omake_exec_local.descriptors local
                  | RemoteServer remote ->
                      Omake_exec_remote.descriptors remote
                  | NotifyServer notify ->
                      Notify.descriptors notify in
              List.fold_left
                (fun fd_table fd ->
                   FdTable.add fd_table fd server
                )
                fd_table fd_set
           )
           FdTable.empty servers in
      let fd_set = FdTable.fold (fun fd_set fd _ -> fd :: fd_set) [] fd_table in
      let fd_set =
         try
            let fd_set, _, _ = Unix.select fd_set [] [] (-1.0) in
               fd_set
         with
            Unix.Unix_error (errno, s1, s2) ->
               eprintf "Select: %s, %s, %s@." s1 s2 (Unix.error_message errno);
               []
      in
      let actions =
        List.map
          (fun fd ->
             let server =
               try Some (FdTable.find fd_table fd)
               with Not_found ->
                 eprintf "Omake_exec.wait_select: fd is unknown: %d@." (Obj.magic fd);
                 None in
             let got_eof =
               match server with
                 | Some { server_handle = LocalServer local ; _} ->
                      Omake_exec_local.handle local options fd
                 | Some { server_handle = RemoteServer remote ; _} ->
                      Omake_exec_remote.handle remote options fd
                 | Some { server_handle = NotifyServer notify ; _} ->
                      Notify.handle notify options fd
                 | None ->
                      false in
             if got_eof then 
               match server with
                 | Some si ->
                      acknowledge_eof si options fd;
                      (fun () -> handle_eof si options fd)
                 | None -> assert false
             else
               (fun () -> ())
          )
          fd_set in
      List.iter (fun f -> f()) actions;
      WaitNone

   (*
    * Thread-based waiting.
    *)
   let start_handler server_info handler (pid_table, fd_table) fd =
      if FdTable.mem fd_table fd then
         pid_table, fd_table
      else (
         let pid = Lm_thread_pool.create true (fun () -> ignore(handler fd)) in
         let fd_table = FdTable.add fd_table fd server_info in
         let pid_table = IntTable.add pid_table pid fd in
            if !debug_thread then
               eprintf "start_handler: %d@." (Lm_unix_util.int_of_fd fd);
            pid_table, fd_table
      )

   let wait_thread server options =
      let { server_servers = servers;
            server_fd_table = fd_table;
            server_pid_table = pid_table;
            _
          } = server
      in

      (* Spawn a thread for each file descriptor *)
      let pid_table, fd_table =
         List.fold_left
           (fun tables server_info ->
              match server_info.server_handle with
                | LocalServer local ->
                    List.fold_left
                      (start_handler
                         server_info
                         (Omake_exec_local.handle local options))
                      tables (Omake_exec_local.descriptors local)
                | RemoteServer remote ->
                     List.fold_left
                       (start_handler
                          server_info
                          (Omake_exec_remote.handle remote options))
                       tables (Omake_exec_remote.descriptors remote)
                | NotifyServer notify ->
                     List.fold_left
                       (start_handler 
                          server_info (Notify.handle notify options)) 
                       tables (Notify.descriptors notify)
           )
           (pid_table, fd_table) servers in
      let pids = Lm_thread_pool.wait () in
      (* A thread finishes when it got an event *)
      let old_fd_table = fd_table in
      let pid_table, fd_table, event_fd_list =
         List.fold_left
           (fun (pid_table, fd_table, event_fd_list) pid ->
              try
                let fd = IntTable.find pid_table pid in
                let pid_table = IntTable.remove pid_table pid in
                let fd_table = FdTable.remove fd_table fd in
                pid_table, fd_table, fd::event_fd_list
              with
                  Not_found ->
                     (* BUG JYH: we seem to be getting unknown pids... *)
                     pid_table, fd_table, event_fd_list
           )
           (pid_table, fd_table, []) pids
      in
      server.server_fd_table <- fd_table;
      server.server_pid_table <- pid_table;
      (* Now that the fd's have been removed from the tables, we can ack any
         eofs seen
       *)
      let ack_eof fd =
        let server_info = FdTable.find old_fd_table fd in
        acknowledge_eof server_info options fd in
      List.iter ack_eof event_fd_list;
      (* Maybe some fd's are at eof: *)
      let hdl_eof fd =
        if !Omake_exec_util.debug_exec then
          eprintf "postprocessing fd %d@." (Lm_unix_util.int_of_fd fd);
        let server_info = FdTable.find old_fd_table fd in
        handle_eof server_info options fd in
      List.iter hdl_eof event_fd_list;
      WaitNone

   (*
    * Wait for all threads to finish.
    *)
   let wait_all server =
      let { server_pid_table = pid_table ; _} = server in
      let rec wait pid_table =
         if not (IntTable.is_empty pid_table) then
            let pids = Lm_thread_pool.wait () in
            let pid_table = List.fold_left IntTable.remove pid_table pids in
               wait pid_table
      in
         wait pid_table;
         server.server_pid_table <- IntTable.empty;
         server.server_fd_table <- FdTable.empty

   (*
    * The wait process handles output from each of the jobs.
    * Once both output channels are closed, the job is finished.
    *)
   let wait ?(onblock=fun() -> ()) server_main options =
      let rec poll servers =
         match servers with
            [] ->
               if likely_blocking server_main then
                 onblock();
               if Lm_thread_pool.enabled then
                  wait_thread server_main options
               else
                  wait_select server_main options
          | server :: servers ->
               let { server_host    = host;
                     server_count   = count;
                     server_running = running;
                     server_handle  = handle;
                     _
                   } = server
               in
               let wait_code =
                  match handle with
                     LocalServer local ->
                        Omake_exec_local.wait local options
                   | RemoteServer remote ->
                        Omake_exec_remote.wait remote options
                   | NotifyServer notify ->
                        Notify.wait notify options
               in
                  match wait_code with
                     WaitInternalExited (id, status, value) ->
                        server.server_running <- pred running;
                        WaitExited (id, status, value)
                   | WaitInternalStarted true ->
                        if opt_print_status options then
                           begin
                              progress_flush ();
                              printf "# server %s started@." host
                           end;
                        server.server_enabled <- true;
                        WaitServer count
                   | WaitInternalStarted false ->
                        if opt_print_status options then
                           begin
                              progress_flush ();
                              printf "# server %s failed@." host
                           end;
                        server.server_enabled <- false;
                        poll servers
                   | WaitInternalNotify event ->
                        WaitNotify event
                   | WaitInternalNone ->
                        poll servers
      in
         poll server_main.server_servers


   (*
    * Ask for a file to be monitored.
    *)
   let monitor { server_notify = notify ; _} node =
      Notify.monitor notify node

   let monitor_tree { server_notify = notify ; _} dir =
      Notify.monitor_tree notify dir

   (*
    * Wait for the next notification.
    * Wait for all threads to complete
    * before issuing this command.
    *)
   let pending server =
      wait_all server;
      Notify.pending server.server_notify

   let next_event server =
      wait_all server;
      Notify.next_event server.server_notify
end

