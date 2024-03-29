# Haskell Project-02
## Library Management System
This is a simple command-line interface (CLI) application for managing a library's inventory. It provides functionalities for managing `members`, `books`, and `borrowing/returning books`. The application is built using `Haskell` and the `state monad` to manage the state of the inventory.

To include `Cabal` in the project, we would need to create a `.cabal` file that specifies the dependencies and other project information. Here we have created `LMS.cabal` file for the same purpose. Once we have created the `.cabal` file, we can use it to build and package our project using tools like `cabal-install`.

Here's the directory structure for the project:
```

library-management-system
├── app
│   └── Setup.hs
├── cabal
│   └── library.cabal
├── src
│   ├── Data
│   │   ├── Book.hs
│   │   ├── Library.hs
│   │   └── Member.hs
│   ├── Main.hs
│   └── Utils.hs
└── test
│   └── Test.hs
└── stack.yaml
└── LICENSE
└── README.md


```

Here's a brief description of the different components:
```
app/Setup.hs - is used by Cabal to build the project.
src/Main.hs - contains the main entry point for the program.
src/Library.hs - contains the implementation of the Library module.
src/Member.hs - contains the implementation of the Member module.
src/Book.hs - contains the implementation of the Book module.
src/Utils.hs - contains utility functions used by other modules.
test/Test.hs - sets up the test environment.
LICENSE - contains the license for the project.
README.md - contains a brief overview of the project.
Library.cabal - specifies the dependencies and other project information.
```
## Modules

### Main.hs
This module contains the main function that runs the library management system. It imports and uses functions from the other modules to display the main menu and execute user-selected operations. The module defines the `InventoryState` data type, which represents the state of the library's inventory. The `initialState` function is defined to provide an initial state for the inventory. The `runApp` function is defined to execute the main logic of the application.

### Member.hs
This module contains the `Member` data type, which represents a member of the library. It also provides functions for `creating`, `updating`, `deleting`, and `displaying member information`. The module defines the `MemberId` type alias for the unique identifier of a member.

### Book.hs
This module contains the `Book` data type, which represents a book in the library. It also provides functions for `creating`, `updating`, `deleting`, and `displaying` book information. The module defines the `BookId` type alias for the unique identifier of a book.

### Library.hs
This module provides functions for managing the inventory of the library. It defines functions for creating members, updating member information, deleting members, creating books, checking out books to members, returning books from members, and printing the inventory.

### Utils.hs
This module provides utility functions for the application, such as functions for reading and writing the inventory to a file, getting user input, and printing formatted output.

### Usage
To run the application, simply execute the following command from the command line:
```
$ stack run
```
This will load the library inventory from the `inventory.txt` file or create a new inventory if the file doesn't exist. The main menu of the application will be displayed, and you can choose the operation you want to perform by entering the corresponding number. Follow the instructions provided by the application to perform the operation.
