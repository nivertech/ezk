------------------------------------------
||>   ezk, the ErlangZooKeeper client  <||
------------------------------------------

-------------------------------------------------
||>   How to use it
-------------------------------------------------
First you have to replace the ZK Server Ips in the 
ezk.app.src with the IPs of your own ZooKeeper server.
Then the client is started like every app. The name of 
the app  is "ezk".

A list of all Commands can be found by using ezk:help().


-------------------------------------------------
||>    Blocking and Non Blocking Commands
-------------------------------------------------

The normal set of commands is blocking, that means, that 
if you call them you need to wait for the answer before
going on. 
Now there is a second set of commands, which start with 
the prefix n_ and are non blocking. The have two additional
parameters: A Pid and a Tag. If the reply to the command
is determinded the Client will send the answer as {Tag, Reply}
to the given PId. Its similar to the usage of watches.
The n_ commands are not fully tested and therefor marked as 
experimental. Commands which set watches are not already 
available in a n_ version.
Using the n_ versions you have to be a little aware of the load.
If you have 1000 processes which send 1000 ls requests to the 
server there is not a problem and the server has to deal  
with at most 1000 messages at one time. But if you use the
n_ls there is 1.000.000 messages which may all arive before
it can answer even the first. This may lead to a delay in 
answering.


-------------------------------------------------
||>    Differences from the Java ZooKeeper Client
-------------------------------------------------
- Watches: 
  	   If you set a watch you specify a WatchOwner 
	   and a WatchMessage as parameters. When the 
	   Watch is triggered the Watchowner (a PId) 
	   gets a Message of the format:
	   {WatchMessage, {WatchedPath, WatchTyp, SyncCon}}. 
	   Or, if the Client loses Connection 
	   {watchlost, WatchMessage, Watchdata}.

- Parallel: 
  	   The Client is ready to handle a high amount 
	   of parallel requests from different Processes.

- Zookeeper:
           A behaviour. For further information... scroll down.


-------------------------------------------------
||>    Things not included
-------------------------------------------------
Quotas are not included in this Client.


-------------------------------------------------
||>    Highlander
-------------------------------------------------
The Highlander, or to be more precise the ezk_highlander, is
a behaviour. You give it a list of ZK Nodenames (which are
not already taken) and a module. Then you start a lot of 
instances of the highlander (on different machines would be 
the normal usecase) and the highlander makes sure there is at 
most one instance per path running and if one fails it is 
directly replaced by another instance.
