       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLMAN6.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   DEALER MASTER FILE MAINTENANCE
      *                 MEMO
      *  REV:
      *   BLV 031897 PUT MEMOS IN F2 WINDOW, NOT AS FIELDS
      *   GLB 020104 REGACC PR# 1691 ADD NEW FIELDS FOR DIRECT DEPOSIT
      *              DL-ACH-FG, DL-BANK-ROUTING-NO, DL-BANK-ACCTNO
      *   BAH 061110 ADDED DL-DBA-NAME TO REDEFINE DL-SIGN3, REGACC PR# 155
      *   BAH 100917 ADDED DL-WAIVE-ACH-FEE FOR MIDATL, PR# 3650
      *   BAH 140310 ADDED DL-TRACK-CHANGE-FLAG FOR REGACC, PR#350
      *   BAH 151023 PASSWORD FIELDS 5 AND 6, BANK ROUTING & NUMBER
      *                                           KSTREET/MULLEN PR# 4816
      *    CS 160225 DL-DBA-NAME IS NOW OWN FIELD X(40).
      * KEC 2016.1221 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FIELD WORKERS TO CONTAIN FILE PREFIXES (DL).
      * BAH 181011 GLOBAL DLFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/DLMAN6.CBL S35 02/21/24-12:22:16 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X        REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1         REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.

       01  CHAN            PIC X(4).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9       VALUE 0.
       01  FIRST-SCAN      PIC 9       VALUE 0.

      *--- VERIFY CHECK DIGIT WORKERS -----------------------------
       01  TMP-BANK-ROUTING-NO   PIC 9(9).
       01  TMP-BANK-ROUT-NO    REDEFINES TMP-BANK-ROUTING-NO.
           03  TMP-RT-NO         PIC 9   OCCURS 9.

       01  WEIGHTING-FACTORS     PIC 9(8)  VALUE 37137137.
       01  WEIGHT-TABLEX       REDEFINES WEIGHTING-FACTORS.
           03 WEIGHT-TABLE       PIC 9    OCCURS 8.

       01  CK-SUB                PIC 9.
       01  TMP-RESULT            PIC 9(3)  VALUE 0.
       01  TMP-SUM               PIC 9(3)  VALUE 0.
       01  CHECK-SUM             PIC 9(3)  VALUE 0.
       01  CHECK-BASE            PIC 9(2)  VALUE 0.
       01  CHECK-MODIFIER        PIC 9     VALUE 0.
       01  WEIGHT-FACTOR         PIC 9(3)  VALUE 0.
       01  CHK-DIG-FG            PIC X     VALUE "Y".
      *------------------------------------------------------------

       01  HOLD-DL1-KEY.
           03  HOLD-DL-BRNO            PIC 9(4).              
           03  HOLD-DL-NUM             PIC 9(6).

       01  ORIGINAL-DL-REC    PIC X(DL-SIZE).
       COPY "LIBLP/LP01DL.CPY" REPLACING LEADING "DL" BY "DLOR".
       01  CHANGED-DL-REC     PIC X(DL-SIZE).

       01  DLMAIN-LINK        VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(6).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  DLMAIN-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  A01         PIC X(25).
           03  A02         PIC X(25).
           03  A03         PIC X(25).
           03  A04         PIC X.
           03  A05         PIC Z(9).
           03  A06         PIC X(17).
           03  A07         PIC X.
           03  A08         PIC X.
           03  A09         PIC X.
           03  A10         PIC X(40).
       01  DISPLAY-LIST.
           03  FILLER          PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100.".

       78  MAX             VALUE 10.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      *  1. SIGNATURE #1
           03  FILLER          PIC X(8) VALUE "E  A250 ".
      *  2. SIGNATURE #2
           03  FILLER          PIC X(8) VALUE "E  A250 ".
      *  3. SIGNATURE #3
           03  FILLER          PIC X(8) VALUE "E  A250 ".
      *  4. DIRECT DEPOSITS PROCEEDS Y/N
           03  FILLER          PIC X(8) VALUE "E  A010 ".
      *  5. BANK ROUTING NUMBER
           03  FILLER          PIC X(8) VALUE "ENNN090 ".
      *  6. BANK ACCOUNT NUMBER
           03  FILLER          PIC X(8) VALUE "E  A170 ".
      *  7. ACH PRENOTE TRANSMITTED FLAG Y/N
           03  FILLER          PIC X(8) VALUE "E  A010 ".
      *  8. WAIVE ACH FEE FLAG Y/N
           03  FILLER          PIC X(8) VALUE "E  A010 ".
      *  9. DEALER TARCK CHANGE FLAG
           03  FILLER          PIC X(8) VALUE "E  A010 ".
      *  9. DBA NAME                    
           03  FILLER          PIC X(8) VALUE "E  A400 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS MAX TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       01  HOLD-SYS-DATE         PIC 9(8)   COMP-3.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  BANK-SEQ                 VALUE 1.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/DLMAN6_DEF.CPY".
       COPY "LIBMN/DLMAN6_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/DLMAN6_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************
      *       MAIN PROGRAM MODULE      *
      **********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DLMAN6" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO DLMAIN-LINK.

           PERFORM SET-HOLD-SYS-DATE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-DL-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

       RETURN-FROM-MEMO.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P1
              MOVE 1 TO FORM-SEL1.

           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE DL-NUM TO SAVEKEY                 
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *******************************
       REC-DL-MODULE SECTION.
      *******************************

           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO DL-NUM                      
              MOVE SPACES TO SAVEKEY
              MOVE "SNNN042DLNO." TO VDU
              MOVE DL-DLNO TO VDUNUM2
              PERFORM SCREENIO
              GO TO READ-DLFILE.

           MOVE 0 TO NEW-FLAG.

       REC-DL-NUMBER.
           MOVE "DLNAME." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-DL-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-DL-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-DL-NUMBER.
           MOVE VDUNUM2 TO DL-DLNO.

           IF DL-DLNO = 0 OR DL-NO = 0
              GO TO REC-DL-NUMBER.

           MOVE DL-BRNO TO HOLD-DL-BRNO.                  
           MOVE DL-NUM  TO HOLD-DL-NUM.                  
       READ-DLFILE.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-DL-MODULE.

           MOVE DL-REC TO ORIGINAL-DL-REC.
           PERFORM DISPLAY-FIELDS.

           MOVE 0 TO NEW-FLAG.

           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.

       END-REC-DL-MODULE.
           EXIT.

      ***********************************
       ENTER-SCANKEY SECTION.
      ***********************************
           MOVE " ENTER DEALER ABRV-NM, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-DL2-FILE.

           MOVE DL-PATH-OWNBR TO DL-BRNO.                
           MOVE SPACES        TO DL-SHORT.               


           PERFORM START-DL2-FILE.
       ENTER-KEY.
           MOVE "E RA160SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE DL-BRNO TO HOLD-DL-BRNO                   
                 MOVE DL-NUM  TO HOLD-DL-NUM                    
                 MOVE DL-REC TO ORIGINAL-DL-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE DL-PATH-OWNBR TO DL-BRNO.                      
           MOVE VDUBUF        TO DL-SHORT.                     

           PERFORM START-DL2-FILE.
       READ-FILES-NEXT.
           PERFORM READ-DL2-FILE-NEXT.
           IF IO-FG = 9 OR (DL-BRNO NOT = DL-PATH-OWNBR)         
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK DL-FILE.
           MOVE "SNNN042DLNO." TO VDU.
           MOVE DL-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-DL2-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE                               *
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD      *
      *              1 - INITIAL NEW RECORD ENTRY    *
      *              2 - MODIFY NEW RECORD           *
      ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              MOVE "D" TO MEMOSC-ONLY
              MOVE DL-BRNO          TO EXT-WI-CL-OWNBR
              MOVE DL-DLNO          TO EXT-WI-CL-DLNO
              MOVE HOLD-SYS-DATE    TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 6
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 1 AND NOT = 2 AND NOT = 3
                                      AND NOT = 4 AND NOT = 5
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.

      * PASSWORD CHANGING BANK ROUTING NUMBER AND BANK ACCOUNT NUMBER
           IF ELE = 5 OR 6
              MOVE BANK-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE BANK INFO:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

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
              GO TO GOTO-FIELD1.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD1.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07
                 A-08 A-09 A-10
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *********************************
      *     SET RECORD ELEMENTS       *
      *********************************
       A-01.
           MOVE VDUBUF TO DL-SIGN1.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO DL-SIGN2.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO DL-SIGN3.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO DL-ACH-FG.
           IF VDUBUF = "Y"
              OR VDUBUF = "N"
                 GO TO CREATE-ROUTINE.
           MOVE "ENTER Y OR N" TO MESS.
           PERFORM SEND-MESS.
           GO TO ENTER-ELE.
       A-05.
           MOVE VDUNUM0 TO DL-BANK-ROUTING-NO.
           PERFORM VERIFY-CHECK-DIGIT.
           IF CHK-DIG-FG = "N"
              MOVE "ROUTING NO. INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO DL-BANK-ACCTNO.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO DL-ACH-PRENOTE-FG.
           IF VDUBUF = "Y"
              OR VDUBUF = "N"
                 GO TO CREATE-ROUTINE.
           MOVE "ENTER Y OR N" TO MESS.
           PERFORM SEND-MESS.
           GO TO ENTER-ELE.
       A-08.
           MOVE VDUBUF TO DL-WAIVE-ACH-FEE.
           IF VDUBUF = "Y"
              OR VDUBUF = "N"
                 GO TO CREATE-ROUTINE.
           MOVE "ENTER Y OR N" TO MESS.
           PERFORM SEND-MESS.
           GO TO ENTER-ELE.
       A-09.
           MOVE VDUBUF TO DL-TRACK-CHANGE-FLAG.
           IF VDUBUF = "Y"
              OR VDUBUF = "N"
                 GO TO CREATE-ROUTINE.
           MOVE "ENTER Y OR N" TO MESS.
           PERFORM SEND-MESS.
           GO TO ENTER-ELE.
       A-10.
           MOVE VDUBUF TO DL-DBA-NAME.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************
       WRITE-MODULE SECTION.
      ******************************************
           PERFORM OPEN-DL1-FILE.
           MOVE DL-REC TO CHANGED-DL-REC.
           IF ORIGINAL-DL-REC = CHANGED-DL-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-DL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF DL-REC NOT = ORIGINAL-DL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE ORIGINAL-DL-REC TO DLOR-REC.
           MOVE CHANGED-DL-REC TO DL-REC.

           IF DL-SIGN1 NOT = DLOR-SIGN1
              MOVE "Y" TO DL-TRACK-CHANGE-FLAG.

           PERFORM REWRITE-DL1-FILE.
       END-WRITE-MODULE.
           PERFORM CLOSE-DL1-FILE.

      ******************************************
       DISPLAY-FIELDS SECTION.
      ******************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  A250DLNAME." TO VDU.
           MOVE DL-NAME TO VDUBUF.
           PERFORM SCREENIO.
           IF NEW-FLAG = 1
              GO TO DISPLAY-FIELDS-EXIT.
           MOVE DL-SIGN1           TO A01.
           MOVE DL-SIGN2           TO A02.
           MOVE DL-SIGN3           TO A03.
           MOVE DL-ACH-FG          TO A04.
           MOVE DL-BANK-ROUTING-NO TO A05.
           MOVE DL-BANK-ACCTNO     TO A06.
           MOVE DL-ACH-PRENOTE-FG  TO A07.
           MOVE DL-WAIVE-ACH-FEE   TO A08.
           MOVE DL-TRACK-CHANGE-FLAG TO A09.
           MOVE DL-DBA-NAME        TO A10.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************
       VERIFY-CHECK-DIGIT SECTION.
      ******************************************
           MOVE DL-BANK-ROUTING-NO TO TMP-BANK-ROUTING-NO.
           MOVE 0 TO CHECK-SUM
                     CHECK-BASE
                     CHECK-MODIFIER
                     WEIGHT-FACTOR
                     TMP-RESULT.
           PERFORM COMPUTE-CHECK-SUM VARYING CK-SUB FROM 1 BY 1
                                     UNTIL CK-SUB > 8.
           DIVIDE 10 INTO CHECK-SUM GIVING CHECK-BASE
                                    REMAINDER CHECK-MODIFIER.

      * CHECK FOR REMAINDER IF Y THEN USE NEXT FACTOR OF 10.

           IF CHECK-MODIFIER NOT = 0
              ADD 1 TO CHECK-BASE.
           MULTIPLY CHECK-BASE BY 10 GIVING WEIGHT-FACTOR.

      * SUBTRACT SUM FROM WEIGHT FACTOR AND COMPARE TO CHECK DIGIT.

           SUBTRACT CHECK-SUM FROM WEIGHT-FACTOR.
           IF WEIGHT-FACTOR = TMP-RT-NO(9)
                MOVE "Y" TO CHK-DIG-FG
           ELSE
                MOVE "N" TO CHK-DIG-FG.
           GO TO EXIT-VERIFY-CHECK-DIGIT.

       COMPUTE-CHECK-SUM.
           MULTIPLY TMP-RT-NO(CK-SUB) BY WEIGHT-TABLE(CK-SUB) GIVING
                                           TMP-SUM.
           ADD TMP-SUM TO CHECK-SUM.

       EXIT-VERIFY-CHECK-DIGIT.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "DLMAN6" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO DLMAN6-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLMAN6-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DLMAN6-SCREEN UPON DLMAIN-WINDOW-HANDLE.
           MOVE DLMAN6-FORM-FIELDS TO WR-BUF.
           MOVE DLMAN6-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE DLMAN6-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLMAN6-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/DLMAN6_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SET-HOLD-SYS-DATE.
           PERFORM GETDAT.
           MOVE SYS-DATE TO HOLD-SYS-DATE.

      ***************************************************
       IO-ROUTINES SECTION.
      ***************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-DL2-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDL1IN.CPY".
       COPY "LIBLP/LPDL2RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***************************************************
       END-ROUTINE SECTION.
      ***************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLMAN6-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE DLMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
