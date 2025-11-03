       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST002.
       AUTHOR.        AMM.
       DATE-WRITTEN.  06-16-2014.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST002.C
      *
      * DESCRIPTION   :  COLLECTION LETTERS
      *                      FMNO 822 - COMAKER LETTER H
      *                           836 - COMAKER LEGAL 1
      *                           840 - COMAKER LEGAL 3/C
      *                           917 - LETTER 8
      *                           918 - LETTER D
      *                           920 - LETTER E
      *                           921 - LETTER F ENGLISH
      *                           923 - LETTER F SPANISH
      *                           926 - BP1
      *                           938 - LETTER E
      *                           944 - LEGAL 7E/C
      *                           946 - LEGAL 7C
      *                           948 - LEGAL 7B
      *                           956 - WELCOME LETTER ENGLISH
      *                           957 - NEW ACCOUNTS ENGLISH
      *                           958 - WELCOME LETTER SPANISH
      *                           959 - NEW ACCOUNTS SPANISH
      *                           981 - 1ST OF MO REMINDER ENG/C
      *                           982 - 1ST OF MO REMINDER SPAN/C
      *
      * CUSTOMER NAMES:  MULLEN FINANCE - K STREET FINANCE
      *
      *=================================================================
      * REV:
      * AMM 2014.0616 {PR#04548} MULLEN/K STREET <A15> [ JUDY ]
      *               **     **
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FP/PST002.CBL S34 06/17/21-11:59:44 >".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBFP/PSTPMCW.CPY".

       COPY "LIBFP/PST_SCREENW.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP FORM-PATHNAME.

           MOVE STAT---GOOD                TO PSTPMC-FORM-STAT.
           MOVE 01                         TO PSTPMC-FORM-PAGE-MAX.

           MOVE "REL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD ) 
              MOVE SPACES                  TO MESS
              STRING "ERROR: 'REL' NOT FOUND (", STAT, ")"
                     DELIMITED BY "  "   INTO MESS
              PERFORM SEND-MESS
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE GETENV-BUF                 TO PSTPMC-FORM-RELEASE.
           MOVE FORM-PROGX                 TO PSTPMC-FORM-PROGRAM.
           MOVE 01                         TO PSTPMC-FORM-PAGE-NO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "PSTPMC"                   TO FORM-PROGX.
           MOVE FORM-PATH                  TO FORM-PATHNAME.

           CALL FORM-PROGX USING FORM-PATH
                                PROG-BUF IP-BUF PRU-PATH
                                PSTPMC-LINK-WORKERS.
           CANCEL FORM-PROGX.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       MISCELLANEOUS-PARA-EXIT.
           MOVE SPACES TO MESS.
           STRING "ERROR: PROGRAM FLOW CONTROL ERROR - ", FORM-NAM
                  DELIMITED BY "  " INTO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           EXIT PROGRAM.
