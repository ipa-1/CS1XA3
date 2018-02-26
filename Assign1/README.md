Assign 1 README


uptodate()

checks for local files that differ from remote repo
as long as there is at least one, the remote repo is not updated

checks for files in remote repo that differ from local repo
as long as there is at least one, the local repo is not updated


uncommitedchanges()
checks for uncommited changes
reference for git diff :https://stackoverflow.com/questions/35978550/how-to-show-uncommitted-changes-in-git
puts differences in local repo that are not in the remote repo, into a file, overwritting data in the file.

findtodo()
finds all files with TODO lines and puts the file name and line into todo.log
reference for exclude :https://stackoverflow.com/questions/221921/use-grep-exclude-include-syntax-to-not-grep-through-certain-files
looks for lines that match #TODO, but exclude the resulting file

haskellerrors()
finds all haskell files then executes a command that checks for errors in the code, and puts the result into error.log.
finds all haskell files in the current folder and all folders in the current folder, and executes a command that checks to see if it runs properly. If it doesnt run properly, it forwards the results to error.log

openalog()
asks user which log file they want to view, depending on the options given
gets input from user, then checks for the value that was given. It will then open a file if the input matches options given

openchat()
interacts with user through asking for input. If the user does not wish to continue script, they may enter "N"
reference for creating a local variable :http://www.linuxjournal.com/content/return-values-bash-functions
reads input and executes the rest of the script depending on input, saves the name input to be used later in the script, as a local variable

myscript()
this function will run all the functions of the script
calls all the function, this function makes it easier to keep track of the different functions of the script 
