# Distributed File System
CS4032 Distributed File System Project.

API and shared libraries used by the system can be found at:
https://github.com/crowley100/my_distro_API

# Language:
Project written in Haskell.

# Usage:
1) Run chmod +x distroFS/start.sh, then run start.sh script from distroFS to boot the system.  
2) Run chmod +x distroFS/use-haskell-client/testRun.sh, then run testRun.sh to see a demo of a handful of commands being run.  
3) The following commands can be used from distroFS/use-haskell-client to interact with the system:  
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
    to file-server/docker-compose.yml, currently two file servers with 1 replica each for testing purposes.  
  - Using an upload/download access model to simplify consistency.
  - User interactions are carried out through a client proxy (use-haskell-client).

2) Security Service:  
  - Client-Service (and sensitive service-service) communication is secured through a 3 key security model.
  - The model assumes a trusted authentication server (AS).
  - To use the systems clients must first be authenticated by the AS, this initial communication is secured
    using an RSA protocol, with the AS' public key widely known. AES session keys are then derived from client passwords,
    along with authentication tickets which are copies of the session key encrypted with the shared service secret.
  - Client can then communicate with various different services by encrypting messages using the session key, and passing
    the authentication ticket along with the message.
  - Services simply need to decrypt the ticket using the shared service secret to obtain the client's session key.
    Using this, they can decrypt that client's communication.

3) Directory Service:  
  - This service is responsible for managing most of the communication within the system.
  - Its primary responsibility is to map human readable, global file names into file identifiers
    used by the file system. It returns these file identifiers to the the client during upload/download
    requests.
  - File servers are treated as directories in this system. That is, files are stored in them as part
    of a flat hierarchy. This way the directory server maintains a mapping of full file names to file ids.
  - It provides the client with a view on the file system through simple list (ls) commnads.
  - Shadowing is also employed on the directory server to ensure consistency of records as transactions
    are carried out or aborted.
  - Finally, it is also responsible for managing replication withing the file system. This functionality ranges
    from keeping records of file servers and replicas, as well as keeping track of which of these are currently
    responding, to load balancing through distribution of read requests among replicas and directing write requests
    to primary servers.

4) Replication:  
  - Replication is largely handled by the directory service as described above.
  - It uses a model based on a primary copy and its replicas.
  - Replicas serve as a method for load balancing as well as functioning as backups for the primary server.
  - In the event of the primary server failing (detected by the directory server), the directory server will
    hold an election to determine which of the available replicas should now serve as the primary copy.

5) Caching:  
  - As this system is modelled on an upload/download approach, caching downloaded files on the client side
    was the primary concern when it came to performance gain through caching.
  - Cache invalidation is carried by the directory service maintaining a timestamp reference for each file
    uploaded to the system. Whenever a client wishses to open a file (to read or write), it first communicates
    with the directory server. If the client's timestamp for that file matches the most up to date one on the directory
    server it can simply use the locally cached copy, otherwise the directory server returns an updated file reference that
    can be used to pull the file from the appropriate file server.

6) Transactions:  
  - A protocol for carrying out transactions (grouping multiple file changes) is implemented as a separate transaction
    service.
  - This transactional modification of files achieved through the use of shadowing. The directory server and file servers in
    the system maintain shadow records. Files uploaded as part of a transaction go through the intermediary step of being stored
    (along with meta data) in these shadow records. If the transaction succeeds, these files are stored in the actual database.
    If the transaction fails these records are deleted.
  - The client can initiate a transaction and proceed with a number of file writes. These writes are stored temporarily on the
    transaction server. The client can then commit or abort the transaction. In the case of a commit, the above shadowing process
    takes place to ensure either all, or none of the transaction is carried out.

7) Lock Service:  
  - The lock server is responsible for providing exclusive access to files while they are being modified.
  - Prior to a file modificating, the client proxy must first attempt to access the lock associated with that file.
  - If the client can aquire the lock, it has exclusive write access for the duration of the modification.
  - At the end of the modification, the client communicates with the lock server to unlock that file.
  - This service is also integrated with transactional file changes. In order to add files to a particular transaction,
    the client must first be able to acquire the lock prior to adding it to the transaction.
  - Then, only when that transaction is comitted or aborted, all the files associated with it are unlocked automatically.

8) Client Proxy:  
  - A simple client proxy (use-haskell-client) has been developed to communicate with the file system.
  - It can access the various functionalites of the system defined in the API.
  - It has been designed to enable it to be included as a library in other applications.
  - A testing script has been included in the use-haskell-client directory to demo the use of the system.
