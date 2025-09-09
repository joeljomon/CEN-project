# InCollege COBOL Project

## What it supports currently

- Input in `data/incollege-input.txt` making sure `data/incollege-output.txt` and `data/accounts.dat` exist
- It will create upto 5 users with password policy enforcement of >8 and <=12 chars, 1 digit, speacial and UPPER req

## Folder Structure

- `src` is where all the code lives
- `src/InCollege.cob` is the main file which references to files like `AccountManagement.cob` and so on
- The main idea is to keep functionalities like password check, input/output seperate and so on seperate
- If adding a new logic like `login` functionalitiy add a `logic.cob` in src/ and reference it in  `InCollege.cob` wherever needed like below:
> An example of `AccountManagement.cob` - how it was referenced in `InCollege.cob`
```
CALL "ACCOUNT-MGMT" USING WS-COMMAND WS-USERNAME WS-PASSWORD WS-MESSAGE
```
> and in AccountManagement.cob define it as 
```
IDENTIFICATION DIVISION.
PROGRAM-ID. ACCOUNT-MGMT.
```


## Run

> Ensure you have docker  
> Clone this repo  
> Open the folder of this repo in VS Code  
> Press `cmd + shift + P`  

![alt text](image.png)

> Choose the option as shown in screenshot  
> Initially try `cmd + shift + B`  
> It should build and automatically get Inputs from data/*-input.txt and log output in data/*-output.txt  


## For Testers

- Create a branch `feature/test-IO` or something with understandable name from this branch
- Go to `src/IO.cob`, you will see lines:
```
           SELECT INPUT-FILE ASSIGN TO "data/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "data/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
```
- Here update `data/*` files to `tests/test1-input.txt` and so on also `tests/test1-output.txt`
- Test all edge cases for account management


