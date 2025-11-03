       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ELMAIN.
      **************************************************************************
      *                 (MN)
      *  DESCRIPTION:   EXTENDED LATE CHARGE CODE
      *                       FILE MAINTENANCE
      *
      * REV:
      *  JTG 050797 ADDED CD-ELUNITPER-CD
      *  KEC 022599 ADDED 'EXITPROG' LOGIC.
      *  BAH 990720 ALLOW ELFRMLA-VALID TO INCLUDE "J" AND "K", JUDY
      *  JTG 030723 ADDED COMMENT TO INSURE THAT CD-EL-CODE WILL NEVER
      *             BE ALLOWED TO BE ZERO, MIDATL
      * KEC 2016.0321 CHANGED HOLD/SAVE KEYS & RECS TO MATCH CD-REC.
      * KEC 2016.0329 CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      * BAH 190130 SPLIT CDFILE
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/ELMAIN.CBL S35 02/23/24-10:20:29 >".

       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  ERRCD           PIC X     VALUE SPACES.
       01  SUB             PIC 999     VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.

       01  HOLD-CD1-KEY                PIC X(10)     VALUE SPACES.
       01  HOLD-CD-EL-KEY              REDEFINES HOLD-CD1-KEY.
           03  HOLD-CD-EL-TYPE         PIC X(2).
           03  HOLD-CD-EL-CODE         PIC 9(2).
           03  HOLD-CD-EL-FILLER       PIC X(6).

       01  CHANGED-CD-REC              PIC X(CD-SIZE).
       01  ORIGINAL-CD-REC             PIC X(CD-SIZE).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01           PIC X.
           03  F02           PIC X.
           03  F03           PIC ZZZ9.9999  BLANK ZERO.
           03  F04           PIC Z9.99      BLANK ZERO.
           03  F05           PIC ZZZ9.99    BLANK ZERO.
           03  F06           PIC Z9.
           03  F061          PIC X.
           03  F07           PIC ZZ9.
           03  F08           PIC ZZZ.
           03  F09           PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 061 070 080 090.".

      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 9.
       01  SPEC-TABLE.
      * 1. CD-ELFRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 2. CD-ELTYPE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 3. CD-ELRATE
           03  FILLER        PIC X(8)   VALUE "ENNN044 ".
      * 4. CD-ELMINCHG
           03  FILLER        PIC X(8)   VALUE "ENNN022 ".
      * 5. CD-ELMAXAMT
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 6. CD-ELGRACE
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 7. CD-ELYRTYPE
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 8. CD-ELDELFAC
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 9. CD-ELUNITPER-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 9 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBWI/ELSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  ELMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/ELMAIN_DEF.CPY".
       COPY "LIBMN/ELMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/ELMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "ELMAIN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ELMAIN-WINDOW-HANDLE.

      D    STOP MESS.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-CD-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ***************************
       REC-CD-MODULE SECTION.
      ***************************
       REC-CD-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "EN N020SOURCE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-CD-MODULE-NO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CD-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-CD-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-CD-MODULE-NO.

           MOVE "EL"    TO CD-TYPE    HOLD-CD-EL-TYPE.
           MOVE VDUNUM0 TO CD-EL-CODE HOLD-CD-EL-CODE.
           MOVE SPACES  TO CD-EL-FILLER HOLD-CD-EL-FILLER.

           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 0
              MOVE CD-REC TO ORIGINAL-CD-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-CD-MODULE.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.
           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00" ) OR ( ELSCAN-CODE  = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNN020SOURCE." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ZEROES        TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
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
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
             MOVE "DEL" TO IO-TYPE
             GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
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
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
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
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09
                 DEPENDING ON ELE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO CD-ELFRMLA.

      * TEMPORARY STOP ON "H" SINCE LONIF1.C NEED TO BE
      * CHANGED TO ALLOW CALC OF CURRENT DUE ON TOTINT:
           IF CD-ELFRMLA = "H"
              GO TO ENTER-ELE.

           IF NOT CD-ELFRMLA-VALID
              MOVE "ENTER: SPACE, 'A' THRU 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF NOT (CD-ELFRMLA = " " OR "B" OR "D" OR "G" OR "K")
              IF CD-ELTYPE = "M"
                 MOVE
           "MUST USE LCFRMLA 'B', 'D', 'G' OR 'K' WITH LC CODE 'M'"
              TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO CD-ELTYPE.
           IF NOT CD-ELTYPE-VALID
              MOVE "ENTER: SPACE, OR 'A' THRU 'O' OR 'Q' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF CD-ELFRMLA = "A"
              IF CD-ELTYPE NOT = "D"
                 MOVE "MUST BE 'D' IF LC FORMULA IS AN 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF CD-ELFRMLA = "B" OR "C" OR "D" OR "J" OR "N"
              IF CD-ELTYPE = "D"
                 MOVE "CAN NOT BE 'D' IF LC FORMULA IS: B,C,D,J,N"
                      TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF CD-ELTYPE = "M"
              IF NOT (CD-ELFRMLA = " " OR "B" OR "D" OR "G" OR "K")
                 MOVE "MUST HAVE LCFRMLA 'B', 'D', 'G' OR 'K'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           PERFORM SEND-ELTYPE-DESC.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM4 TO CD-ELRATE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO CD-ELMINCHG.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO CD-ELMAXAMT.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM0 TO CD-ELGRACE.
       F-061.
           MOVE "E  A010061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-061.
           MOVE VDUBUF TO CD-ELGRACECD.
           IF NOT CD-ELGRACECD-VALID
              MOVE "ENTER: SPACE, S'UNDAY', OR W'EEKEND'" TO MESS
              PERFORM SEND-MESS
              GO TO F-061.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM0 TO CD-ELYRTYPE.
           IF CD-ELYRTYPE = 0
              IF CD-ELFRMLA = " "
                 GO TO CREATE-ROUTINE.
           IF NOT CD-ELYRTYPE-VALID
              MOVE "ENTER: 358, 360, 365, OR 367" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM0 TO CD-ELDELFAC.
           IF NOT CD-ELDELFAC-VALID
              MOVE "ENTER: 0 - 100" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO CD-ELUNITPER-CD.
           IF NOT (CD-ELUNITPER-CD = " " OR "A")
              MOVE "ENTER: SPACE OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      **********************************
       WRITE-MODULE SECTION.
      **********************************
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

      **********************************
       DELETE-MODULE SECTION.
      **********************************
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

      **********************************
       NEW-RECORD-SETUP SECTION.
      **********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-CD-FILE.

           MOVE HOLD-CD1-KEY TO CD1-KEY.

           MOVE 0 TO ELE.

      **********************************
       DISPLAY-FIELDS SECTION.
      **********************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE CD-ELFRMLA TO F01.
           MOVE CD-ELTYPE TO F02.
           MOVE CD-ELRATE TO F03.
           MOVE CD-ELMINCHG TO F04.
           MOVE CD-ELMAXAMT TO F05.
           MOVE CD-ELGRACE TO F06.
           MOVE CD-ELGRACECD TO F061.
           MOVE CD-ELYRTYPE TO F07.
           MOVE CD-ELDELFAC TO F08.
           MOVE CD-ELUNITPER-CD TO F09.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-ELTYPE-DESC.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCDCL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO ELMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO ELMAIN-SYSDATE.
           DISPLAY ELMAIN-SCREEN UPON ELMAIN-WINDOW-HANDLE.
           MOVE ELMAIN-FORM-FIELDS TO WR-BUF.
           MOVE ELMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE ELMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ELMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/ELMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-ELTYPE-DESC.
           MOVE "S  A070021." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF CD-ELTYPE = " "
              MOVE "N/A" TO VDUBUF
           ELSE IF CD-ELTYPE = "A"
              MOVE "% REGPM" TO VDUBUF
           ELSE IF CD-ELTYPE = "B"
              MOVE "$ AMT" TO VDUBUF
           ELSE IF CD-ELTYPE = "C"
              MOVE "% OF LATE" TO VDUBUF
           ELSE IF CD-ELTYPE = "D"
              MOVE "% PER MO" TO VDUBUF
           ELSE IF CD-ELTYPE = "E"
              MOVE "% RGPM TRUNC" TO VDUBUF
           ELSE IF CD-ELTYPE = "F"
              MOVE "% RGPM RNDUP" TO VDUBUF
           ELSE IF CD-ELTYPE = "G"
              MOVE "% PMT" TO VDUBUF
           ELSE IF CD-ELTYPE = "H"
              MOVE "% PMT TRUNC" TO VDUBUF
           ELSE IF CD-ELTYPE = "I"
              MOVE "% PMT RNDUP" TO VDUBUF
           ELSE IF CD-ELTYPE = "J"
              MOVE "% OF WLATE" TO VDUBUF
           ELSE IF CD-ELTYPE = "K"
              MOVE "% OF RLATE" TO VDUBUF
           ELSE IF CD-ELTYPE = "L"
              MOVE "% OF WRLATE" TO VDUBUF
           ELSE IF CD-ELTYPE = "M"
              MOVE "% LATE - MF" TO VDUBUF
           ELSE IF CD-ELTYPE = "N"
              MOVE "% WHOLE-MF" TO VDUBUF
           ELSE IF CD-ELTYPE = "Q"
              MOVE "FLAT CHARGE" TO VDUBUF.
           PERFORM SCREENIO.
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

      **********************************
       IO-ROUTINES SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **********************************
       END-ROUTINE SECTION.
      **********************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ELMAIN-SCREEN.
           DESTROY ELMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
