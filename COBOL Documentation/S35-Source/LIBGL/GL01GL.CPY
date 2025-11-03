      * COPYMEMBER: LIBGL/GL01GL
      **************************************************
      *          GLFILE - GENERAL LEDGER MASTER  
      *
      * GL1-KEY = GL-BRANCH, GL-ACCOUNT
      * GL2-KEY = GL-DESCRIPTION
      *
      * REV:
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  BLM 090120 ADDED GL-SPANISH-DESC, WORLD PR# 616
      *  BAH 151204 ADDED FILLER, ADDED GL-LTOUCH-DATE, PD0005
      *  BAH 190219 REMOVED GL-BAL(), GL-BUD() REDEFINES, NOT USED
      *             REMOVED GL-DEPT, GL-ACCT REDEFINES, NOT USED
      *  KEC 190319 CHANGED, ONCE AGAIN, THE FORMATTING OF THE GL1-KEY.
      *             AFTER A DISCUSSION WITH ALAN I MADE IT WHERE
      *             GL1-KEY WAS 9(11) THEN BROKEN OUT INTO 4 & 7.
      *             REASON IS THAT THE OTHER FILES (AL/FS/GI/JD) THAT
      *             USE THE FULL 11 BYTE ACCOUNT NUMBER DOES NOT BREAK
      *             IT DOWN.  SO KEEPING IT CONSISTENT IN THE SYSTEM
      *             AND IN THE SQL DATABASE.
      *             FOR CONSISTENY THE NAME WILL BE XX-BR-ACCOUNT IN
      *             THE OTHER FILE AS WELL.
      *  CS 190514 REMOVE ENDING FILLER, PER KEVIN, SHOULD BE IN WORKDAY BY
      *            A30, BUT ADDED A FEW FUTURE FIELDS
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20210421 REMOVED FUTURE FIELDS, CHANGED GL1-KEY TO BE GROUP
      *  20230600 BAH ADDED FILLER
      **************************************************
       78  GL-SIZE                 VALUE 580.
       01  GL-REC.
           03  GL-FLAG             PIC X.

           03  GL1-KEY.
               05  GL-BR-ACCOUNT       PIC 9(11).
               05  GL-BR-ACCOUNTX REDEFINES GL-BR-ACCOUNT.
                   07  GL-BRANCH       PIC 9(4).
                   07  GL-ACCOUNT      PIC 9(7).
      $XFD DATE=YYYYMMDD
           03  GL-LTOUCH-DATE      PIC 9(8)      COMP-3.

           03  GL2-KEY.
               05  GL-DESCRIPTION  PIC X(36).
           03  GL-CONSOL           PIC 9(4).
           03  GL-OPENBAL          PIC S9(9)V99 COMP-3.

           03  GL-AMOUNTS.
               05  GL-CYBAL        PIC S9(9)V99 COMP-3 OCCURS 12.
               05  GL-LYBAL        PIC S9(9)V99 COMP-3 OCCURS 12.
               05  GL-CYBUD        PIC S9(9)    COMP-3 OCCURS 12.
               05  GL-LYBUD        PIC S9(9)    COMP-3 OCCURS 12.
               05  GL-NYBUD        PIC S9(9)    COMP-3 OCCURS 12.

           03  GL-TYPE             PIC X.
           03  GL-CLEAR            PIC X.
           03  GL-NUMBER1          PIC 9(7).
           03  GL-NUMBER2          PIC 9(7).
           03  GL-FS-FLAG          PIC X.
           03  GL-REPORTS          PIC X               OCCURS 5.
           03  GL-LYOPENBAL        PIC S9(9)V99 COMP-3.
           03  GL-DLALLOC-GL       PIC X.
           03  GL-SPANISH-DESC     PIC X(36).

           03  FILLER              PIC X(128).
