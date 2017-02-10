# Distributed File System
CS4032 Distributed File System Project.

API and shared libraries used by the system can be found at:
https://github.com/crowley100/my_distro_API

# Language:
Project written in Haskell.

# Usage:
1) Run chmod +x distroFS/start.sh, then run start.sh script from distroFS to boot the system.  
2) Run testRun.sh script to see a demo of a handful of commands being run.  
3) The following commands can be used from distroFS/use-haskell-client  
  - to interact with the system:  
    - stack exec use-haskell-client-exe sign-up name pass  
    - stack exec use-haskell-client-exe log-in name pass  
    - stack exec use-haskell-client-exe ls-dir  
    - stack exec use-haskell-client-exe ls-file dirName  
    - stack exec use-haskell-client-exe upload fileName dirName  
    - stack exec use-haskell-client-exe download fileName dirName  
    - stack exec use-haskell-client-exe begin-transaction  
    - stack exec use-haskell-client-exe commit  
    - stack exec use-haskell-client-exe abort  

# Architecture Overview:
1) Distributed Transparent File Access:  
  - More file servers can be run, along with additional replicas by adding them
    to file-server/docker-compose.yml, currently a small amount for testing purposes.  
2) Security Service:  
3) Directory Service:  
4) Replication:  
5) Caching:  
6) Transactions:  
7) Look Service:  
8) Client Proxy:  
