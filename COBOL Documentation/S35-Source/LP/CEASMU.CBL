       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CEASMU.
       DATE-WRITTEN.    2024.1030.
      *********************************************************************
      *                 (LP)
      *  DESCRIPTION:  CEASE AND DESIST MENU
      *                19. ON CHANGES MENU (LONPX0)
      * REV:
      *  BAH 2024.1030 NEW FOR WORLD CEASE & DESIST MENU #1686 S35Q-145
      *********************************************************************
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
       77  SCCS-IDENTIFIER PIC X(39) VALUE
           "@(#)LP/LONPX0.C A15 09/26/23-14:58:31 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  FORM-BUF                    PIC 9(6) VALUE 0.
       01  FORM-LIST                   PIC X(4) VALUE "SEL.".
       01  MSEL-LIST                   PIC X(5) VALUE "MSEL.".
 
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CEASMU_DEF.CPY".
       COPY "LIBLP/CEASMU_WKS.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME            PIC X(22).
       01  EXIT-PATHNAME            PIC X(22).
  
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/CEASMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      /*******************************
      *  C O N T R O L   M O D U L E *
      ********************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
 
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CEASMU"      TO VDU-FORM-NAME.

           MOVE SPACES TO ERRCD.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       GET-SEL.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM FORM-RESET.

           MOVE SPACES    TO WR-BUF.
           MOVE FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "ENTER SELECTION, F7-MENU:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO EXIT-PROG.
           IF VDUSTATUS NOT = "00"
              GO TO GET-SEL.

           MOVE VDUNUM0 TO FORM-BUF.

           GO TO M01 M02 
                DEPENDING ON FORM-BUF.

           MOVE "LP/CEASMU" TO FORM-NAM.
           MOVE FORM-PATH  TO FORM-PATHNAME.

           GO TO GET-SEL.
       M01.
           MOVE "LP/CEASUP" TO FORM-NAM.
           GO TO GET-PROG.
       M02.
           MOVE "LP/CEASRM" TO FORM-NAM.
           GO TO GET-PROG.


       GET-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CEASMU-SCREEN.
      **************************************************************
      * THE SECURITY CHECK HERE IS HARD-CODED INSTEAD OF COPYING
      * LIBGB/GETFM.  COPY MEMBER GOES TO EXIT-PROG ON ERROR & THE
      * SECURITY ERROR MSG IS NOT DISPLAYED, EVEN THO IT STOPS
      * YOU FROM RUNNING SELECTION.  NOTE, THE SAME THING HAPPENED
      * IN A15, SECURITY WORKED BUT NO MSG.  I THINK THIS IS A
      * SPECIAL SITUATION SINCE THIS IS CALLED BY LONP.  BARB
      * 1/17/17 REMOVE SEND-MESS WITH SECURITY ERROR, THIS IS IN
      *         CHKSEC SO WE WERE GETTING IT TWICE.  BARB
      **************************************************************
       CHECK-SECURITY.
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           MOVE "CO" TO FORM-OBJ.
           MOVE "CL/CHKSEC" TO FORM-NAM.
           CALL "CHKSEC" USING CHKSEC-FIELDS ON OVERFLOW
                MOVE CHKSEC-FORM-PATH TO FORM-PATH
                MOVE "3Z" TO FORM-OBJ
                GO TO EXIT-PROG
           END-CALL.

           CANCEL "CHKSEC".
           MOVE CHKSEC-FORM-PATH TO FORM-PATH.
           IF CHKSEC-STAT = "3S"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO GET-SEL.
           IF CHKSEC-STAT = "3O"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO GET-SEL.

       COPY "LIBGB/LOADCOLP.CPY".

       LOAD-POST.
           MOVE "LP/CEASMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           GO TO GET-SEL.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
 
      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CEASMU_EVA.CPY".
               WHEN OTHER   MOVE "2A" TO STAT.

      ***********************************
       SETUP-LINK-INFO SECTION.
      ***********************************
           MOVE CEASMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE CEASMU-HELP--DIR TO HELP-LINK-DIR.

      ***********************************
       FORM-RESET SECTION.
      ***********************************
           MOVE LPLOAN-SYSDATE TO CEASMU-SYSDATE.
           MOVE LPLOAN-CONAME  TO CEASMU-CONAME.
           DISPLAY CEASMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CEASMU-FORM-FIELDS TO WR-BUF.
           MOVE CEASMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************
       MISC SECTION.
      **********************************
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
            MOVE "E" TO ERRCD.
            MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
       END-RUN.
       EXIT-PROG.
       LOAD-RESTART.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CEASMU-SCREEN.
       EXIT-RETURN.
           EXIT PROGRAM.
