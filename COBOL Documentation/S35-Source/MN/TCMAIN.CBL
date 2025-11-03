       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TCMAIN.
      **************************************************************************
      *                 (MN)
      *  DESCRIPTION:   PAYMENT POST TRANSACTION REFERENCE CODES
      *
      * REV:
      * CS  040805  **NEW **    LENDMARK [PR#2353]
      *     040809 ASSOCIATED REFNO CODES FOR VARIOUS PAYMENT POST CODES
      * KEC 2016.0321 CHANGED HOLD/SAVE KEYS & RECS TO MATCH CD-REC.
      * KEC 2016.0329 CHANGED TCSCAN WORKERS TO MATCH CD1-KEY FOR TC LAYOUT.
      * BAH 190128 SPLIT CDFILE
      **************************************************************************
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
           "@(#)MN/TCMAIN.CBL S35 02/23/24-10:19:47 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

       01  HOLD-CD1-KEY                PIC X(10)     VALUE SPACES.

       01  CHANGED-CD-REC              PIC X(CD-SIZE).
       01  ORIGINAL-CD-REC             PIC X(CD-SIZE).

       01  ERRCD           PIC X VALUE " ".
       01  CHAN        PIC X(4).
       01  SUB             PIC 99  VALUE 0.
       01  S1              PIC 99 VALUE ZERO.
       01  S2              PIC 99 VALUE ZERO.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  MSEL-LIST       PIC X(5)   VALUE "MSEL.".
       01  LEGEND          PIC X(60).

       01  LEGEND-01.
           03  FILLER      PIC X(42) VALUE
           "F7-EXIT, F6-SCAN, ENTER TRANSACTION CODE: ".

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF       VALUE " ".
           03  F01           PIC X(5)  OCCURS 10.
           03  F02           PIC X(12).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(24) VALUE
           "010 020 030 040 050 060 ".
           03  FILLER      PIC X(20) VALUE
           "070 080 090 100 110.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 11.
       01  SPEC-TABLE.

      * 1. CD-TC-REFNO(1)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      * 2. CD-TC-REFNO(2)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      * 3. CD-TC-REFNO(3)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      * 4. CD-TC-REFNO(4)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      * 5. CD-TC-REFNO(5)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      * 6. CD-TC-REFNO(6)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      * 7. CD-TC-REFNO(7)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      * 8. CD-TC-REFNO(8)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      *09. CD-TC-REFNO(9)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      *10. CD-TC-REFNO(10)
           03  FILLER      PIC X(8)   VALUE "E  A050 ".
      *11. CD-DESC
           03  FILLER      PIC X(8)   VALUE "E  A120 ".

       01  SPEC-TABLE2    REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 11.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBWI/TCSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  TCMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TCMAIN_DEF.CPY".
       COPY "LIBMN/TCMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/TCMAIN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************
      *    MAIN PROGRAM MODULE
      ******************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TCMAIN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TCMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-TC-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

       ENTRY-MODULE-REPEAT.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM REMOVE-GAPS
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.
           GO TO MAIN-MODULE.

      **************************************
       REC-TC-MODULE SECTION.
      **************************************
           MOVE 0 TO NEW-FLAG.
           MOVE " " TO IO-TYPE.

           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.

      * ENTER PAYMENT POSTING CODE

           MOVE "E  A020TCCODE." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              PERFORM TCSCAN-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-TC-MODULE
              ELSE
                 MOVE "S  A020TCCODE." TO VDU
                 MOVE TCSCAN-CODE TO VDUBUF
                 PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
             MOVE "END" TO IO-TYPE
             GO TO END-REC-TC-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO REC-TC-MODULE.

           MOVE VDUBUF  TO CD-CODE.
           IF CD-CODE = SPACES
              MOVE "TRANSACTON CODE CAN NOT BE SPACES" TO MESS
              PERFORM SEND-MESS
              GO TO REC-TC-MODULE.

           MOVE "TC"   TO CD-TYPE.
           MOVE CD1-KEY TO HOLD-CD1-KEY.

      * READ CDFILE:
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 0
              MOVE CD-REC TO ORIGINAL-CD-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-TC-MODULE.
           EXIT.

      **************************************
       REMOVE-GAPS SECTION.
      **************************************

      * REMOVE GAPS FROM THE 10 LEVELS SO VALID REFNO CODES ARE
      * DISPLAYED FIRST

           MOVE 0 TO S2.
           MOVE 0 TO S1.

       SET-TARGET-NEXT.
           ADD  1 TO  S2.

           IF S2 > 10
              GO TO REMOVE-GAPS-EXIT.

           IF CD-TC-REFNO(S2) NOT = "~~~~~"
              GO TO SET-TARGET-NEXT.

           MOVE S2 TO S1.

       SET-SOURCE-NEXT.
           ADD 1 TO S1.

           IF S1 > 10
              GO TO REMOVE-GAPS-EXIT.

           IF CD-TC-REFNO(S1) = "~~~~~"
              GO TO SET-SOURCE-NEXT.

           MOVE CD-TC-REFNO(S1) TO CD-TC-REFNO(S2).
           MOVE "~~~~~" TO CD-TC-REFNO(S1).


           GO TO SET-TARGET-NEXT.

       REMOVE-GAPS-EXIT.
           EXIT.

      **************************************
       TCSCAN-SCAN SECTION.
      **************************************

           MOVE "TC"        TO TCSCAN-TYPE.
           MOVE SPACES      TO TCSCAN-CODE.

           MOVE "WI/TCSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH TCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF TCSCAN-CODE = SPACES 
              MOVE 9 TO IO-FG
              GO TO TCSCAN-SCAN-EXIT.

           MOVE 0 TO IO-FG.
           MOVE "00" TO VDUSTATUS.

       TCSCAN-SCAN-EXIT.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = " "
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
             MOVE "DEL" TO IO-TYPE
             GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL " "
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0   TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01
                 F-01 F-01 F-01 F-01 F-01
                 F-01 F-01 F-01 F-01 F-02
                 DEPENDING ON ELE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           IF VDUBUF NOT = "~~~~~"
               IF VDUBUF NOT = CD-TC-REFNO(ELE)
                 PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 10
                   IF VDUBUF =  CD-TC-REFNO(S1)
                     MOVE "DUPLICATE REFNO'S NOT ALLOWED" TO MESS
                     PERFORM SEND-MESS
                     GO TO ENTER-ELE
                   END-IF
               END-PERFORM.

           MOVE VDUBUF TO CD-TC-REFNO(ELE).

           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUBUF TO CD-DESC.
           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-CD1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE CD-REC TO CHANGED-CD-REC.
           IF ORIGINAL-CD-REC = CHANGED-CD-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF CD-REC NOT = ORIGINAL-CD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-CD-REC TO CD-REC.
           PERFORM REWRITE-CD1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-CD1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-CD1-FILE.

      ************************************************
       DELETE-MODULE SECTION.
      ************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF CD-REC NOT = ORIGINAL-CD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-CD1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-CD1-FILE.

      ************************************************
       NEW-RECORD-SETUP SECTION.
      ************************************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-CD-FILE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE "~~~~~" TO CD-TC-REFNO(SUB)
              MOVE "~~~~~" TO F01(SUB)
           END-PERFORM.

           MOVE HOLD-CD1-KEY TO CD1-KEY.
           MOVE SPACES       TO CD-DESC.
           MOVE 0 TO ELE.

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE " " TO DISPLAY-BUF.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE CD-TC-REFNO(SUB)   TO F01(SUB)
           END-PERFORM.
           MOVE CD-DESC            TO F02.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCDCL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO TCMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO TCMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TCMAIN-SCREEN UPON TCMAIN-WINDOW-HANDLE.
           MOVE TCMAIN-FORM-FIELDS TO WR-BUF.
           MOVE TCMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE TCMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TCMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/TCMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.


      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TCMAIN-SCREEN.
           DESTROY TCMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
