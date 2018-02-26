#!/bin/bash

# checks to see if repository is up to date
uptodate(){
	echo " ***** Checking for local/remote updates .. *****"
	local="$(git diff --name-only | wc -l)"
	if [ "$local" -gt 0 ]
	then
		echo " "
		echo "Oh no! Your local repo has files that have not been updated to the remote repo: "
		git diff --name-only
		echo " "
	fi

        remote="$(git diff --name-only origin master | wc -l)"
        if [ "$remote" -gt 0 ]
	then
		echo " "
		echo "Oh no! Your remote repo has files on it that you don't have yet: "
		git diff --name-only origin master
		echo " "
	fi
}

# checks for uncommited changes
# reference for git diff :https://stackoverflow.com/questions/35978550/how-to-show-uncommitted-changes-in-git
uncommitedchanges(){
	echo " ***** Creating changes.log .. *****"
	git diff --name-status > changes.log
}

# finds all files with TODO lines and puts the file name and line into todo.log
# reference for exclude :https://stackoverflow.com/questions/221921/use-grep-exclude-include-syntax-to-not-grep-through-certain-files
findtodo(){
	echo " ***** Creating todo.log .. *****"
	grep -r "#TODO" --exclude="todo.log"  > todo.log
}

#finds all haskell files then executes a command that checks for errors in the code, and puts the result into error.log.
haskellerrors(){
	echo " ***** Creating error.log .. *****"
	find . -name "*.hs" -exec ghc -fno-code {} \; &> error.log
}

# asks user which log file they want to view, depending on the options given
openalog(){
	read -p " Hello! Which log do you want to see? : changes, todo, or error " file 
	if [ "$file" == "changes" ];then
		cat changes.log
		openalog
	elif [ "$file" == "todo" ];then
		cat todo.log
		openalog
	elif [ "$file" == "error" ];then
		cat error.log
		openalog
	else
		echo "It was fun knowing you!"
	fi
}

# interacts with user through asking for input. If the user does not wish to continue script, they may enter "N"
# reference for creating a local variable :http://www.linuxjournal.com/content/return-values-bash-functions
openchat(){
	read -p "My name's Curtis! What's your name? " name
	echo "Hello "$name""
	read -p "Would you like me to evaluate your repository? y/n " consent
	if [ "$consent" == "n" ]
		then 
			echo "consent is important!"
			exit 0
	else
		local name="$consent"
	fi
}

# this function will run all the functions of the script
myscript(){
	openchat
	uptodate
	uncommitedchanges
	findtodo
	haskellerrors
	openalog
	echo "Goodbye $name "	
}



#run the script
myscript
