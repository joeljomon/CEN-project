# InCollege COBOL Project — EPIC 5

## What it supports currently

- Input is read from **`data/InCollege-Input.txt`**  
  Make sure these files exist:
  - `data/InCollege-Input.txt` (test script)
  - `data/InCollege-Output.txt` (logs)
  - `data/accounts.dat`
  - `data/profiles.dat`
  - `data/connections.dat`
  - `data/pending.dat`
- Create up to 5 users with password policy (8–12 chars, ≥1 uppercase, ≥1 digit, ≥1 special).
- Each user can create **one** profile (fields validated).
- Data persists across runs in `data/*.dat`.

---

## EPIC 5 Features (Week 5)

### Connection Request Management (Developer 1)
- **Accept Pending Requests**: Users can accept connection requests to establish permanent connections
- **Reject Pending Requests**: Users can reject connection requests without creating connections
- **Request Processing**: 
  - Displays each pending request individually
  - Prompts user to accept (1) or reject (2)
  - Removes processed requests from `pending.dat`
  - Shows confirmation message for each action
- **Connection Storage**: Accepted requests are stored in `connections.dat`

### Network Display (Developer 2)
- **View My Network**: New menu option to display all established connections
- **Connection Details**: Shows connected user's name, university, and major (when available)
- **Bidirectional Display**: Connections work in both directions (if A connects to B, both see each other)

### Menu Structure
Main Menu now includes:
1. Create/Edit User Profile
2. View User Profile
3. Search for User
4. Learn a new skill
5. **View My Pending Connection Requests** (updated with accept/reject)
6. **View My Network** (new)
7. Log Out

---

## EPIC 4 Features (Previous Week)

- Search user by first + last name (case-insensitive)
- **Send Connection Request** from profile view
- **Duplicate protection**:
  - Cannot send a request to yourself
  - Cannot send if you already sent one (outgoing duplicate)
  - Cannot send if they already sent you one (incoming duplicate)
  - Cannot send if already connected
- **View My Pending Connection Requests** (display only)

---

## File Structure

### Source Files (`src/`)
- `InCollege.cob` - Main program
- `IO.cob` - Input/Output module
- `AccountManagement.cob` - User account creation/login
- `Utilities.cob` - Password validation
- `Navigation.cob` - Post-login menu navigation
- `CreateEditProfile.cob` - Profile creation/editing
- `ViewProfile.cob` - Profile display
- `SearchUser.cob` - User search functionality
- `ConnectionRequest.cob` - Send connection requests with duplicate checking
- `PendingRequests.cob` - View and manage pending requests (accept/reject)
- `AddConnection.cob` - Store accepted connections
- `NetworkDisplay.cob` - Display established network
- `SkillMenu.cob` - Skills learning menu
- `login.cob` - Login functionality

### Data Files (`data/`)
- `accounts.dat` - User credentials
- `profiles.dat` - User profile information
- `pending.dat` - Pending connection requests (format: `sender->receiver`)
- `connections.dat` - Established connections (format: two 20-char username fields)
- `InCollege-Input.txt` - Test input script
- `InCollege-Output.txt` - Program output log

---

## How to Build and Run
```bash
# Clean previous builds
make clean

# Compile all modules
make

# Run the program (reads from data/InCollege-Input.txt)
./bin/InCollegeApp

# Or use the convenience target
make run