# Distributed File System
CS4032 Distributed File System Project.

API and shared libraries used by the system can be found at:
https://github.com/crowley100/my_distro_API

# Language:
Project written in Haskell.

# Usage:
1) Run chmod +x distroFS/start.sh, then run start.sh script from distroFS to boot the system.\n
2) Run testRun.sh script to see a demo of a handful of commands being run.\n
3) The following commands can be used from distroFS/use-haskell-client\n
   to interact with the system:\n
    • stack exec use-haskell-client-exe sign-up name pass\n
    • stack exec use-haskell-client-exe log-in name pass\n
    • stack exec use-haskell-client-exe ls-dir\n
    • stack exec use-haskell-client-exe ls-file dirName\n
    • stack exec use-haskell-client-exe upload fileName dirName\n
    • stack exec use-haskell-client-exe download fileName dirName\n
    • stack exec use-haskell-client-exe begin-transaction\n
    • stack exec use-haskell-client-exe commit\n
    • stack exec use-haskell-client-exe abort\n

# Architecture Overview:
1) Distributed Transparent File Access:\n
  • More file servers can be run, along with additional replicas by adding them
    to file-server/docker-compose.yml, currently a small amount for testing purposes.
2) Security Service:\n
3) Directory Service:\n
4) Replication:\n
5) Caching:\n
6) Transactions:\n
7) Look Service:\n
8) Client Proxy:\n
