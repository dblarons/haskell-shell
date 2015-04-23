## Hash - Haskell, a Shell; or, Hashkell

#### Team Members with their vunetid

Aaron Smith `smithah4`

#### Project Description

- Run a command in foreground. [DONE]

- Builtins

  - `cd`: change directory has to be run on the main thread, otherwise the
    directory doesn't get changed because the thread that changed directory
    gets killed. [DONE]

  - `exit`: exit needs to be run on the main thread, for the same reasons that
    cd needs to be run there. [DONE]

  - `help`: help needs to be a builtin because it is specific to my
    application. [DONE]

  - `|` (pipe operator): the pipe operator will be a builtin. It will take the
    text output from one command and pass it to another command. [DONE]

  - `>` (I/O redirection operator): the redirection operator will allow the
    user to take stdout and redirect it into a file. Can redirect between
    files. [DONE]

  - Environment variable management: `set`, `unset`, `list`, etc. [DONE]

- Config file: Allow configuration of a PATH variable and other, user-defined,
  variables to be loaded when a shell is spawned. [DONE]

- Maybe some auto complete (Dr. Otte says: "See readline"). [DONE]

- Run a command in the background and return the control back to the terminal.
  [DONE]

#### Error Conditions

- failed foreground commands should not auto-restart and should instead simply
  notify the user. [DONE]

- If concurrent background processes are running and one fails, have some way
  to identify it so that the user knows which of the background processes
  failed. [DONE]

#### Milestones

March 24 – Submit a report about the design.

April 9 – Status update and submit a set of slides describing what I have done
up to this point.

April 21 – Final report.

#### Expected Schedule/Timeline

March 24 - Finish the first bullet point.

April 9 - Finish the second and third bullet points.

April 18 - Have the final bullet point finished.

#### Language and OS Used

Haskell. I'm writing on OSX, but it should work on Linux distros also.

#### Project setup

1. Clone this repo.

2. Install the [Haskell Platform](https://www.haskell.org/platform/)

3. Run `cabal configure` to configure the project without tests and `cabal
   configure --enable-tests` to configure with tests.

4. Optionally install Guard (a Ruby plugin) to enable automatic running of
   tests when files are saved.

5. Install readline bindings: 

    a. `brew install readline`

    b. `cabal install readline
    --extra-include-dirs=/usr/local/Cellar/readline/6.3.8/include/
    --extra-lib-dirs=/usr/local/Cellar/readline/6.3.8/lib/
    --configure-option=--with-readline-includes=/usr/local/Cellar/readline/6.3.8/include/
    --configure-option=--with-readline-libraries=/usr/local/Cellar/readline/6.3.8/lib/`

