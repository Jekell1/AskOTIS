       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDMAIN.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-11-2000.
      ******************************************************************
      *
      * IDENTIFICATION:  MN/PDMAIN.C
      *
      * DESCRIPTION   :  SCRATCH PAD FILE MAINTENANCE
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000911 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB AND BETH
      * MJD 131127 A30 CONVERSION COMPLETED
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/PDMAIN.CBL A30 12/20/16-14:35:34 >".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  EXT-EOBUF-MESS                  PIC X(60)     VALUE SPACES.

       01  PROG-WK.
           03  FILLER                  PIC X(07)     VALUE "MN/PDPG".
           03  PROG-NO                 PIC 9(02)     VALUE 01.

      *****************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       01 FORM-LINK-AREA     PIC X(2000).

       COPY "LIBGB/LOADCOW.CPY".
       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME      PIC X(22).

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM INITIALIZATION.

       MAIN-PROGRAM-LOAD.

           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.

           MOVE FORM-LINK-AREA TO PDMAIN-LINK.
           IF ( ERRCD = "E" )
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           IF ( ERRCD = "X" )
              GO TO END-PROGRAM.

           MOVE FORMSEL TO PROG-NO.

           GO TO MAIN-PROGRAM-LOAD.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
      D    STOP MESS.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-GOOD )
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/PDPG01"   TO FORM-NAM.
           MOVE FORM-PATH     TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS PDMAIN-WINDOW-HANDLE.
            MOVE PDMAIN-LINK TO FORM-LINK-AREA.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       EXIT-PROG.

           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY PDMAIN-WINDOW-HANDLE
           EXIT PROGRAM.

