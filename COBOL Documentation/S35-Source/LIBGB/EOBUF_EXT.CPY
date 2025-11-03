      * COPYMEMBER: LIBGB/EOBUF.EXT
      ******************************************************
      *           (EOBUF)
      *    BATCH PROCEDURE MEMBER
      *    NOTE:
      *         FOLLOW THIS WITH REPORT PARAMETERS
      *
      *     EX. COPY LIBGB/EOBUF.EXT.
      *             03  AGE-DATE        PIC 9(8).
      *             03  BEG-BRANCH      PIC 99.
      *             03  END-BRANCH      PIC 99.
      *             03  BEG-CLASS       PIC 99.
      *             03  END-CLASS       PIC 99.
      * MJD 130108 ADDED EXT-EOBUF-WINDOW-HANDLE.
      *            MADE EXTERNAL BECAUSE PROGRAM WINDOW IS OPENED IN
      *            EOMAIN OR IN THE REPORT/LISTING DEPENDING ON ROUTE-BUF
      * BAH 130412 ADDED EXT-EOBUF-ERRCD EXTERNAL FOR ALL BATCHABLE PROGRAMS
      * BAH 130514 CHANGED EXT-EOBUF-PRPATH TO 40 FROM 30, FULL PATH STUFF
      * BLM 170310 CHANGED EXT-EOBUF-DATE1 & EXT-EOBUF-DATE2 TO 9(8),
      *            INCREASE EXT-EOBUF-DATA FROM 400 TO 600, PD0006
      * BAH 2024.0308 REMOVED QUEUE-BUF, RKEY-BUF, RDESC, RFIRST
      ******************************************************

      *01  QUEUE-BUF              PIC X.
      *01  PRU-BUF                PIC 99.

       01  NOACCEPT-BUF           PIC X.

      *01  RKEY-BUF               PIC 99    VALUE 0.
      *01  RDESC                  PIC X(16).
      *01  RFIRST                 PIC X     VALUE "Y".

      * LASTNEXT-BUF IS ONLY REFERENCED BY MOVING SPACES TO IT. THE FIELDS UNDER
      * IT ARE USED IN 5 PROGRAMS; GREXEC, EOEXEC, EOCRON, AUTOC2 AND EORUN
      * LAST-PROGRAM GETS SPACES MOVED TO IT INITIALLY. IT HAS AN 'IF' THAT
      * COULD MOVE WS-PGM-NAME. IT IS NEVER USED TO POPULATE ANOTHER FIELD.
      * NEXT-PROGRAM IS NEVER POPULATED, ALWAYS SPACES. ONLY USED TO POPULATE
      * A SCROLL ARRAY LEVEL FOR MAYBE A DISPLAY SOMEWHERE.
      * THESE NEED A DEEPER LOOK.

       01  LASTNEXT-BUF EXTERNAL.
           03  LAST-PROGRAM       PIC X(9).
           03  NEXT-PROGRAM       PIC X(9).

      *******************************************
      *    BATCH PROCEDURE BUFFER
      *******************************************

       01  EXT-EOBUF-WINDOW-HANDLE    PIC X(10) EXTERNAL.

       01  EXT-EOBUF-PAGE             PIC X(8) EXTERNAL.
       01  EXT-EOBUF-TITLE            PIC X(40) EXTERNAL.

       01  EXT-EOBUF-ERRCD            PIC X EXTERNAL.
           88  EXT-EOBUF-ERRCD-GOOD         VALUE SPACES.
           88  EXT-EOBUF-ERRCD-BAD          VALUE "!" THRU "~".

      * EXT-EOBUF-PCODE  IS THE BATCH NAME BEING RAN
      * EXT-EOBUF-PRPATH IS MOVED TO PRU-FILE AND EO-SPOOL
      * EXT-EOBUF-COPIES IS NOT USED BUT TOO AFRAID TO REMOVE AS THERE ARE 
      *                  REDEFINES THAT DO NOT MAKE SENSE ON THEIR SIZES
       01  EXT-EOBUF-BUF EXTERNAL.
           03  EXT-EOBUF-PCODE        PIC XXX.
           03  EXT-EOBUF-PRPATH       PIC X(50).
           03  EXT-EOBUF-COPIES       PIC 9.
           03  EXT-EOBUF-DATE1        PIC 9(8).
           03  EXT-EOBUF-DATE2        PIC 9(8).
           03  EXT-EOBUF-MESS         PIC X(60).
           03  EXT-EOBUF-MESS-FERROR REDEFINES EXT-EOBUF-MESS.
               05  EXT-EOBUF-MESS-TYPE PIC XXX.
               05  EXT-EOBUF-MESS-CODE PIC X(7).
               05  EXT-EOBUF-MESS-PATH PIC X(50).
           03  EXT-EOBUF-DATA         PIC X(600).

      * DO NOT KNOW WHAT THE X(680) DOES OR WHERE IT GETS YOU ? AFRAID TO TOUCH
      * THIS
       01  EOBUFX                 REDEFINES EXT-EOBUF-BUF.
           03  FILLER                 PIC X(680).
           03  EXT-EOBUF-GROUP-PRPATH PIC X(45).
           03  EXT-EOBUF-GROUP-FG     PIC X.
           03  EXT-EOBUF-GROUP        PIC X(4).
           03  EXT-EOBUF-BRNO REDEFINES EXT-EOBUF-GROUP PIC 9(4).

      * VERSION MUST BE WHERE EXT-EOBUF-DATA STARTS
       01  EOBUFZ                 REDEFINES EXT-EOBUF-BUF.
           03  FILLER                 PIC X(130).
           03  EXT-EOBUF-VERSION      PIC X.
