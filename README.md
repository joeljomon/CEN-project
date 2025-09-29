# InCollege COBOL Project — EPIC 4

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
- **Epic 4 features**:
  - Search user by first + last name (case-insensitive).
  - **Send Connection Request** from profile view.
  - **Duplicate protection**:
    - Cannot send a request to yourself.
    - Cannot send if you already sent one (outgoing duplicate).
    - Cannot send if they already sent you one (incoming duplicate).
    - Cannot send if already connected.
  - **View My Pending Connection Requests** (menu item shows all requests where you are the receiver).

---

## Folder Structure

