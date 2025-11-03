      * COPYMEMBER: LIBGB/SYSTEMW
      ******************************************************************
      *
      *    SYSTEMW:  WORKERS FOR EXECUTING SYSTEM COMMANDS
      *
      *=================================================================
      ***
      *** LIBGB/CSYSTEM.CPY & LIBGB/CSYSTEMW.CPY WERE COPY MEMBERS THAT
      *** USED C$SYSTEM. WERE CREATED BY BARB & CINDY BACK IN 2013 WHEN
      *** CREATING A30 FROM A15. CHANGES WERE DONE FOR VERYANT BUT WERE
      *** REVERSED LATER.  THERE WERE SOME DISPLAY ISSUES DEPENDING ON
      *** ITS USAGE.
      *** LIBGB/CSYSTEM.CPY & LIBGB/CSYSTEMW.CPY WERE MOVED TO THE
      *** SV DIRECTORY AS A .NO SINCE THEY WERE NOT USED.
      *** THERE ARE HARDCODED C$SYSTEM IN GB/QUEPRU, GB/PREPRU AND
      *** GB/POSPRU DUE TO GARBAGE:
      ***   "THIS MUST STAY C$SYSTEM OR GARBAGE PRINTS, NO IDEA WHY"
      *** FOR MORE INFO, LOOK IN THE SHARED DRIVE FOR BARB'S DOC:
      ***   == PROJECT DOCUMENTS==/2).PROJECT DEVELOPMENT/2).SYSTEM CALL
      ***   SYSTEM CALL.DOCX 
      ***                                        UPDATE BY JAY 2017-0919
      ***
      *=================================================================
      * REV:
      * JTG 010596 CHANGED LENGTH OF BUFFER TO 90 TO STOP OVERRUN
      *            OF NULL CHARACTER INTO NEXT FIELD OF COBOL PROGRAM
      *            WHEN PID'S ARE LONGER THAN 5 CHARACTERS
      * JTG 021213 ADDED SYSTEM-STAT PIC 9(3) FOR ACU CALL "SYSTEM"
      * BLM 130325 REVISED ROUTINE FOR A30, CHANGED TO CALL "C$SYSTEM"
      *            & PIPE ERRORS TO /DEV/NULL, TRASH SYSTEM-STAT
      *
      * KEC 2017.0925 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED RM-CMD FOR USE IN ALL PROGRAMS.
      *
      ******************************************************************
       01  CAT-CMD                     PIC X(3)   VALUE X'636174'.
       01  CP-CMD                      PIC X(2)   VALUE X'6370'.
       01  EC-CMD                      PIC X(2)   VALUE X'6563'.
       01  LS-CMD                      PIC X(2)   VALUE X'6C73'.
       01  MKDIR-CMD                   PIC X(5)   VALUE X'6D6B646972'.
       01  MV-CMD                      PIC X(2)   VALUE X'6D76'.
       01  RM-CMD                      PIC X(2)   VALUE X'726D'.

      * 2>/DEV/NULL
       01  DEV-NULL-CMD.
           03  FILLER        PIC X(4) VALUE " 2>/".
           03  SYSTEM-DEVNULL.
               05  FILLER    PIC X(3) VALUE X'646576'.
               05  FILLER    PIC X    VALUE "/".
               05  FILLER    PIC X(4) VALUE X'6E756C6C'.

       01  SYSTEM-BUFFER.
           03  SYSTEM-BUF            VALUE " ".
               05  SYSTEM-COMMAND    PIC X(257).
           03  SYSTEM-BUFX REDEFINES SYSTEM-BUF.
               05  S-C1              PIC X(60).
               05  S-C2              PIC X(60).
               05  S-C3              PIC X(60).
               05  S-C4              PIC X(64).
      * 2>/DEV/NULL
               05  SYSTEM-DEV-NULL   PIC X(12).
               05  FILLER            PIC X.
           03  RM-BUF                REDEFINES SYSTEM-BUF.
               05  FILLER            PIC X(4).
               05  RM-FILE           PIC X(253).
           03  MKDIR-BUF             REDEFINES SYSTEM-BUF.
               05  FILLER            PIC X(6).
               05  MKDIR-FILE        PIC X(251).
           03  CAT-BUF               REDEFINES SYSTEM-BUF.
               05  FILLER            PIC X(4).
               05  CAT-FILE          PIC X(253).
           03  MV-BUF                REDEFINES SYSTEM-BUF.
               05  FILLER            PIC X(4).
               05  MV-FILE1          PIC X(40).
               05  FILLER            PIC X.
               05  MV-FILE2          PIC X(40).
               05  FILLER            PIC X(172).

