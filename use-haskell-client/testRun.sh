#!/bin/bash
# demo script for distroFS
printf "\n --- This is a simple script whose purpose is to demo some of the interaction in the system ---\n"
sleep 3

printf "\n --- SIGNING UP: testName testPass ---\n"
stack exec use-haskell-client-exe sign-up testName testPass
sleep 3

printf "\n --- LOGGING IN: testName testPass ---\n"
stack exec use-haskell-client-exe log-in testName testPass
sleep 4

printf "\n --- LISTING DIRECTORIES ---\n"
stack exec use-haskell-client-exe ls-dir
sleep 4

printf "\n --- CHECKING CONTENTS: fs1 ---\n"
stack exec use-haskell-client-exe ls-file fs1
sleep 4

printf "\n --- UPLOADING: myTest.txt  TO: fs1 ---\n"
stack exec use-haskell-client-exe upload myTest.txt fs1
sleep 4

printf "\n --- REMOVING LOCAL COPY: myTest.txt ---\n"
rm myTest.txt
ls
sleep 4

printf "\n --- DOWNLOADING: myTest.txt  FROM: fs1 ---\n"
stack exec use-haskell-client-exe download myTest.txt fs1
ls
sleep 5

printf "\n --- STARTING A TRANSACTION ---\n"
stack exec use-haskell-client-exe begin-transaction
sleep 3

printf "\n --- UPLOADING: trans1.txt  TO: fs2  AS PART OF A TRANSACTION ---\n"
stack exec use-haskell-client-exe upload trans1.txt fs2
sleep 4

printf "\n --- UPLOADING: trans2.txt  TO: fs2  AS PART OF A TRANSACTION ---\n"
stack exec use-haskell-client-exe upload trans2.txt fs2
sleep 4

printf "\n --- COMMITTING TRANSACTION ---\n"
stack exec use-haskell-client-exe commit
sleep 4

printf "\n --- REMOVING LOCAL COPY: trans1.txt ---\n"
rm trans1.txt
ls
sleep 4

printf "\n --- CHECKING CONTENTS: fs2 ---\n"
stack exec use-haskell-client-exe ls-file fs2
sleep 4

printf "\n --- DOWNLOADING: trans1.txt  FROM: fs2 ---\n"
stack exec use-haskell-client-exe download trans1.txt fs2
ls
