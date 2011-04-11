-ifndef(ezk_ezk_HRL).
-define(ezk_ezk_HRL,1).
-define(LOG, ezk_log:put).

-record(cstate, {open_requests = dict:new(), 
		 socket :: port(), 
		 ip, 
		 port :: 0..65535, 
		 timeout, 
		 sessionid, 
		 iteration :: pos_integer(),
		 outstanding_heartbeats = 0,
		 outstanding_auths = 0,
		 watchtable
 	       }).

-record(zkmsg, {cmd  :: watchevent | ls | create | delete | get | set,
		msgid :: pos_integer(),
		zxid  :: pos_integer(),
		payload :: binary()
	       }).

-record(con_man_state, {defaultserverlist,
		       connections,
		       monitorings
		       }).

-record(high_state, {is_active = false,
		     ident,
		     my_path,
		     module, 
		     parameters,
		     wait_for_active = [],
		     module_state,
		     connection_pid
		    }).

-endif.
