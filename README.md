# InCollege COBOL Project — EPIC 8

## EPIC 8: Messaging System (Part 1 – Send Message)

This release introduces the basic messaging feature that allows connected users to send private messages within the InCollege application.

### Features

**Messaging Menu**
- Added a new **Messages** option to the main post-login menu.
- Includes two sub-options:
  - **Send a New Message** — implemented in this release.
  - **View My Messages** — currently under construction.

**Message Sending**
- Allows users to send messages only to people they are already connected with.
- Validates the recipient’s username against existing connections.
- Displays an error if the recipient is not found or not connected.
- Prompts the sender to type a short free-form message.
- Confirms successful delivery after sending.

**Message Storage**
- Saves all sent messages persistently to a file.
- Each message record includes the sender, recipient, and message content.
- Messages remain saved even after the program is closed.

**Input / Output**
- All user inputs (menu choices, usernames, messages) are read from an input file.
- All outputs (prompts, errors, confirmations) are displayed on screen and also written to an output file.

---

## Technical Implementation

**Modified Files:**
- `InCollege.cob` — Added “Messages” menu integration and flow handling.
- `Navigation.cob` — Updated main menu to include the new option.

**New Files:**
- `MessageHandler.cob` — Handles message input, validation, and file writing.
- `messages.dat` — Stores all sent message records.

**Data Flow:**
1. User logs in and selects **Messages** from the main menu.
2. Chooses **Send a New Message** and enters a connected username.
3. Types message content.
4. Message is written to the messages file and a confirmation is displayed.

---

## Build & Run
```bash
make clean && make
./bin/InCollegeApp
