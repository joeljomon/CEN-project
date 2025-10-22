# InCollege COBOL Project — EPIC 5

## EPIC 5: Connection Management

This release adds the ability to manage connection requests and view your professional network.

### Features

**Connection Request Management**
- View all pending connection requests sent to you
- Process each request individually with accept/reject options
- Accepting a request:
  - Creates a permanent bidirectional connection
  - Stores the connection in connections.dat
  - Removes the request from pending.dat
- Rejecting a request:
  - Removes the request from pending.dat without creating a connection
- Clear confirmation messages after each action

**Network Display**
- View your complete network of established connections
- Displays each connection with their profile information (name, university, major)
- Works bidirectionally - if you're connected to someone, they see you in their network too

### New Menu Options
- **Option 5**: View My Pending Connection Requests (now with accept/reject)
- **Option 6**: View My Network (new)

---

## Technical Implementation

**Modified Files:**
- `PendingRequests.cob` - Added accept/reject logic and temporary file handling for request removal
- `Navigation.cob` - Fixed module call to match NETWORKDISPLAY program name
- `NetworkDisplay.cob` - Bug fixes (added missing EOF variable, corrected file paths)

**New Files:**
- `AddConnection.cob` - Stores accepted connections in proper format

**Data Flow:**
1. User views pending requests (reads from `data/pending.dat`)
2. User accepts/rejects → calls AddConnection if accepted
3. Request removed from pending.dat using temp file swap method
4. View Network reads from `data/connections.dat` and cross-references `data/profiles.dat`

---

## Build & Run
```bash
make clean && make
./bin/InCollegeApp
