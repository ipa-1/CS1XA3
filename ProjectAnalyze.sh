#!/bin/bash

uptodate(){
	echo " ********** Checking if local repo is up to date with the remote repo .. **********"
	if [$(git diff --name-only | wc -l) -gt "0"]
	then
		echo " Oh no! Your local repo has files that have not been updated to the remote repo"
	fi
	if [$(git diff --name-only origin master | wc -l) -gt 0]
	then
		echo " Oh no! Your remote repo has files on it that you don't have yet!"
	fi
}

uncommitedchanges(){
	echo " ********** Putting all uncommited changes into changes.log .. **********"
	git diff --name-status > changes.log
}

findtodo(){
	echo " ***** Finding all the files with #TODO lines and putting into todo.log .. **********"
	grep -r "#TODO" --exclude=todo.log > todo.log
}

haskellerrors(){
	echo " ***** Putting the haskell file errors into error.log .. **********"
	find . -name "*.hs" -exec ghc -fno-code {} \; &> error.log
}

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
