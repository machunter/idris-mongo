
# If mongo drivers are not installed:

brew update
curl -LO https://github.com/mongodb/mongo-c-driver/releases/download/1.12.0/mongo-c-driver-1.12.0.tar.gz
tar xzf mongo-c-driver-1.12.0.tar.gz
cd mongo-c-driver-1.12.0
brew install cmake
mkdir cmake-build
cd cmake-build
cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF ..
make
sudo make install

# export paths
export CPATH=/usr/local/include/libmongoc-1.0:/usr/local/include/libbson-1.0:/usr/local/lib/
export LD_LIBRARY_PATH=/usr/local/lib/

#build
idris --build mongolib.ipkg

# start mongodb
mongod -vvvvv --config /usr/local/etc/mongod.conf

# Reminder

* Typechecking (ctrl-alt-r)
  * compiles the file and reports errors
* Case-splitting (ctrl-alt-c)
  * split a variable which can be pattern matched
* Clause-adding (ctrl-alt-a)
  * add a clause to a function
* Proof-search (ctrl-alt-s)
  * search for a proof of a hole
* Showing the types of a variable (ctrl-alt-t)
  * show the type of a hole
* Show the doc for a function (ctrl-alt-d)
* make-with (ctrl-alt-w)
  * add further variables on the left hand side of a function
* make-case (ctrl-alt-m)
* make-lemma (ctrl-alt-l)
  * lift a hole into a function context
* Add proof case (ctrl-alt-p)
  * alternate version of clause adding when trying to proof a type. http://docs.idris-lang.org/en/latest/reference/misc.html#match-application
* Browse namespace (ctrl-alt-b)
  * select the name of a namespace beforehand
* Showing holes
* ipkg highlighting
* REPL (ctrl-alt-enter)
* Apropos view
