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



-------------------------------------------------
||>    Things not included
-------------------------------------------------
Quotas are not included in this Client.

