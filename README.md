## Hash - Haskell, a Shell; or, Hashkell

#### Team Members with their vunetid

Aaron Smith `smithah4`

#### Project Description

- Run a command in foreground. [DONE]

- Builtins

  - `cd`: change directory has to be run on the main thread, otherwise the
      directory doesn't get changed because the thread that changed directory
      gets killed. [DONE]

  - `exit`: exit needs to be run on the main thread, for the same
      reasons that cd needs to be run there. [DONE]

  - `help`: help needs to be a builtin because it is specific to my
      application. [DONE]

  - `|` (pipe operator): the pipe operator will be a builtin. It will take the
      text output from one command and pass it to another command.
      https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/unix-2.4.0.1/System-Posix-IO.html#8

  - `>` (I/O redirection operator): the redirection operator will allow the
      user to take stdout and redirect it into a file. Can redirect between
      files.

  - Environment variable management: `set`, `unset`, `list`, etc.

- Config file: Allow configuration of a PATH variable and other,
    user-defined, variables to be loaded when a shell is spawned. The 
    config file must be written in Haskell, of course.

- Maybe some auto complete (Dr. Otte says: "See readline").

- Run a command in the background and return the control back to the terminal.

- Run a background command with auto recovery mode i.e. the program is automatically restarted if it dies due to any reason other than a SIGSEGV or a SIGKILL.

- Support multiple concurrent background processes.

#### Error Conditions

- background process fails; should restart, but report to user somehow.

- failed foreground commands should not auto-restart and should instead
simply notify the user.

- If concurrent background processes are running and one fails, have some way
to identify it so that the user knows which of the background processes
failed. 

#### Milestones

March 24 – Submit a report about the design.

April 9 – Status update and submit a set of slides describing what I have done up to this point.

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

