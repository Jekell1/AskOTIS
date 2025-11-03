       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PCPGRM.
       AUTHOR.         KEC.
       DATE-WRITTEN.   06-30-2006.
      ******************************************************************
      *
      * IDENTIFICATION:  MN/PCPGRM.C
      *
      * DESCRIPTION   :  SCRATCH PAD FILE (PC) FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * KEC 2006.0630 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ***NEW***
      *
      * MJD 131202 A30 CONVERSION COMPLETED
      * AMM 140501 FIXED EXIT-PATHNAME, WAS USING EXITPROG, VERYANT
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/PCPGRM A32 01/30/20-08:22:40 >".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  EXT-EOBUF-MESS                  PIC X(60)     VALUE SPACES.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       01  FORM-LINK-AREA       PIC X(2000).
       COPY "LIBMN/PCPGRMW.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S E C T I O N
      *
      ******************************************************************
       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }
      D    STOP MESS.
           PERFORM INITIALIZATION.

      *=================================================================
       LOAD-PRE.

           MOVE PROGRAM-FORM               TO FORM-NAM.

       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.

           MOVE FORM-LINK-AREA TO PCPGRM-LINK-BUF.

           IF ( ERRCD = "X" )
              GO TO END-PROGRAM.

           IF ( ERRCD = "E" )
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           MOVE PAGE-SELECTION  TO PROGRAM-FORM-PAGE.

           GO TO LOAD-PRE.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-GOOD )
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE "01"                       TO PAGE-SELECTION
                                              PROGRAM-FORM-PAGE.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE PROGRAM-FORM               TO FORM-NAM.
           MOVE FORM-PATH                  TO FORM-PATHNAME.

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
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS PCPGRM-WINDOW-HANDLE.
           MOVE PCPGRM-LINK-BUF TO FORM-LINK-AREA.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY PCPGRM-WINDOW-HANDLE.
           EXIT PROGRAM.
