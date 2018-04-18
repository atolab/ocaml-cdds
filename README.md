# ocaml-cdds
This is the OCaml API for [Cyclone DDS](http://github.com/atolab/cdds).

## Installing ocaml-cdds
To install ocaml-dds make sure you to have installed:

- [Cyclone DDS](http://github.com/atolab/cdds)
- [OCaml](http://ocaml.org) and [Opam](https://opam.ocaml.org)

Once the dependencies listed above are satisfied, you'll have to install a few OCaml tools and packages by executing the following command:

    opam install ctypes ctypes-foreign jbuilder
    

Then from the **ocam-cdds** directory do the following:

    $ ./configure # on Linux you'll have to be sudoer
    $ make
    $ make install

That's it.

## ocaml-dds examples
To build  the example shipped with ocaml-cdds do the following:

    $ make demo
    
Then to run the example, on one terminal do:

    $ ./demo/simple/_build/default/simple.exe sub-wl
    
Then to run the example, on another terminal do:

    $ ./demo/simple/_build/default/simple.exe pub
    
    
You should see data being printed on the console showing what the subscrier is receiving.


 


