This project is a simple library, into which I have moved the REST API of the use-haskell project in order to
demonstrate how one may create a library that REST clients and servers can use to access and serve REST APIs
respectively.

If you look in the respective project `stack.yml` files, you will see that this library is included as a git respoitory.

Thus if you change the API and push the changes to the repository, you can then  include the updated API in
your `use-haskell` service and `use-haskell-client` projects to enable client/server communication. You achiee this by
updating your stack.yml file to refer to the apporpriate commit. Note also that if you do not update teh commit id then
you will continue to use the original API while compiling your service. This allows independent teams to develop and
upgrade to an evolving API on a timeline they prefer. 

Note then, that this approach only works if you maintain an online git repository and push API changes in this library
to it. Howeer, once you do this, and updates t