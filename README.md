# Haskell-Project-02
For Cardano Developer Program - **Library Management System**

The above program is a library management system that is set up as a **Cabal** project with multiple modules. It uses custom types to represent books and members in the library.

The Cabal file sets up the library management system as an executable project with the main file as **Main.hs** and other modules being **Library.hs**, **Book.hs**, and **Member.hs**. The build dependency is on the base package, with a minimum version of **4.12** and less than version **5**.

The main file, **Main.hs**, is the entry point of the program and provides a simple menu-driven interface for users to interact with the library management system. The menu provides options to list books, check out a book, return a book, list members, and register a member. The selected option is processed using a case statement and calls the appropriate function from the other modules.

The **Library.hs** module defines the custom type Library, which is a list of Books, and implements the functions to list books, check out a book, and return a book. The library is initialized with a list of books and their details. The *listBooks* function prints the details of all the books in the library. The *checkOutBook* function allows the user to check out a book by entering its name, and the *returnBook* function allows the user to return a book by entering its name. The *findBook* function is used to find a book in the library by its name, and the *checkOutBook* and *returnBook* functions are used to update the status of a book in the library.

The **Book.hs** module defines the custom type Book, which has three fields: name, author, and check-out status. The Book type is used in the **Library.hs** module to represent the books in the library.

The **Member.hs** module implements the functions to manage members of the library. This module has not been implemented in the above code, but you could add functions to register a member, list members, and update the details of a member.

Overall, this program provides a basic implementation of a library management system, with the ability to list books, check out a book, and return a book. 
