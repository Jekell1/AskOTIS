       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDTALY.
       DATE-WRITTEN.    060184.
      *****************************************************
      *                      (LP)
      *      DESCRIPTION: FORM PRINT REQUEST TALLY
      *
      *  BAH 181114 GLOBAL FMFILE
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/FDTALY.CBL S35 02/21/24-11:42:43 >".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".

       01  MESS-LIST            PIC X(5)  VALUE "MESS.".
       01  TOT-WAITING          PIC 9(6)  VALUE 0.
       01  TOT-PRINTED          PIC 9(6)  VALUE 0.

       01  TALLY-BUF            VALUE " ".
           03  T-FMNO           PIC ZZ9.
           03  T-TITLE          PIC X(25).
           03  T-WAITING        PIC ZZZ,ZZ9.
           03  T-PRINTED        PIC ZZZ,ZZ9.
       01  TALLY-LIST           PIC X(12) VALUE
           "FM TI WA PR.".


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/FDLIST_DEF.CPY".
       COPY "LIBLS/FDLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  LINK-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  FORM-NO         PIC 9(3).
           03  BRANCH          PIC 9(4).
           03  FDLIST-WINDOW-HANDLE PIC X(10).

       SCREEN SECTION.
       COPY "LIBLS/FDLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME LINK-BUF.

      **********************************
       MAIN-MODULE SECTION.
      **********************************

           PERFORM SQL-CONNECT.

      * DOES NOT HAVE ITS OWN FORM!!!! CALLED FROM LS/FDLIST
           MOVE "FDLIST" TO VDU-FORM-NAME.
      D    STOP MESS.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.
       NEXT-FM.
           PERFORM READ-FM2-FILE-NEXT.
           IF IO-FG = 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-ROUTINE.

           IF FM-FMNO NOT = FORM-NO
              GO TO END-ROUTINE.

           IF BRANCH NOT = 0
              IF FM-BRNO NOT = BRANCH
                 GO TO NEXT-FM.

           IF FM-PRINT = "Y"
              ADD 1 TO TOT-PRINTED
           ELSE
              ADD 1 TO TOT-WAITING.

           GO TO NEXT-FM.

       END-ROUTINE.
           MOVE " " TO MESS.
           PERFORM PROGRESS-MSG.

           MOVE FD-FMNO TO T-FMNO.
           MOVE FD-DESC TO T-TITLE.
           MOVE TOT-WAITING TO T-WAITING.
           MOVE TOT-PRINTED TO T-PRINTED.
           MOVE TALLY-BUF TO WR-BUF.
           MOVE TALLY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.

           PERFORM OPEN-FD1-FILE.
           MOVE FORM-NO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
              MOVE "FORM CODE NOT ESTABLISHED" TO MESS
              PERFORM SEND-MESS
              MOVE "?" TO ERRCD
              GO TO EXIT-INITIALIZE.

           PERFORM OPEN-FM2-FILE.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE FORM-NO       TO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.

           PERFORM START-FM2-FILE.

           MOVE "TALLY IN PROGRESS................." TO MESS.
           PERFORM PROGRESS-MSG.

       EXIT-INITIALIZE.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO FDLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FDLIST-SYSDATE.
           DISPLAY FDLIST-SCREEN UPON FDLIST-WINDOW-HANDLE.
           MOVE FDLIST-FORM-FIELDS TO WR-BUF.
           MOVE FDLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.



      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE FDLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDLIST-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/FDLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ***********************************
       MISC-PARAGRAPHS SECTION.
      ***********************************
       PROGRESS-MSG.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FD1-FILE.
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************
       END-PROGRAM SECTION.
      ******************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
