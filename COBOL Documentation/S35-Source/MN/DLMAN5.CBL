       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLMAN5.
      ******************************************************************
      *                 (MN)
      *  DESCRIPTION:   DEALER MASTER FILE MAINTENANCE
      *
      *                 PAGE 5
      *
      * REV:
      *  BAH 022398 ADDED DL-DISC-FRMLA 5 FOR FFG, PR# 653,
      *             AMTFIN - OWNED INS & FEES
      *  JTG 000217 ADDED DL-DISC-CODE-NO-PL-EXP            ACE #980
      *   CS 060411 ADDED SAL-MISC-PCT & SAL-MISC-PCT2 IF GP-I-AM-SAL SAL#2503
      *   CS 070106 CORRECT DISPLAY/ACCESS TO DL-SAL-MISC-PCT& DL-SLA-MISC-PCT2
      *      FOUND BY LORI WHEN TESTING DEALER STATEMENT PERCENTS PR#2503 SAL
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
           "@(#)MN/DLMAN5.CBL S35 02/21/24-12:21:22 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01  SUB             PIC 99      VALUE 0.
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
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.

       01  HOLD-DL1-KEY.
           03  HOLD-DL-BRNO            PIC 9(4).
           03  HOLD-DL-NUM             PIC 9(6).

       01  ORIGINAL-DL-REC    PIC X(DL-SIZE).
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
           03  DLNAME      PIC X(30).
           03  A01         PIC Z9.
           03  A02         PIC X.
           03  DL-DISC-TABLE    OCCURS 6.
               05  ADISCTERM    PIC ZZZ.
               05  ADISCRATE    PIC ZZ.ZZZZ.
           03  A09         PIC 99.
           03  A10         PIC X.
           03  A11         PIC ZZZ.ZZ.
           03  A12         PIC ZZZZZ.ZZ-.
           03  A13         PIC ZZZ9.
           03  A14         PIC ZZZ9.
           03  A15         PIC ZZZ9.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(47) VALUE
           "DLNAME 010 020 030 031 040 041 050 051 060 061 ".
           03  FILLER      PIC X(50) VALUE
           "070 071 080 081 090 100 110 120 130 140 150.".

       01  MAX          PIC 99   VALUE 15.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      *01. DEALER DISCOUNT FORMULA
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *03. DEALER DISCOUNT BREAKS (1 TO 6)
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      *09. SERVICE ACCOUNT PARAMETERS
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      *13. LATE CHARGE PERCENT
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      *14. INTEREST PERCENT
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      *15. DEFERMENT PERCENT
           03  FILLER      PIC X(8) VALUE "ENNN030 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 15 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/DLMAN5_DEF.CPY".
       COPY "LIBMN/DLMAN5_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/DLMAN5_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      **********************************
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DLMAN5" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO DLMAIN-LINK.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM REC-DL-MODULE.


           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 6 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE DL-NUM TO SAVEKEY 
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      *******************************
       REC-DL-MODULE SECTION.
      *******************************
       REC-DL-MODULE-NO.
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
       ENTER-SCAN.
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
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 5
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 1 AND NOT = 2 AND NOT = 3
                                      AND NOT = 4 AND NOT = 6
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
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *********************************
      *     SET RECORD ELEMENTS       *
      *********************************
       A-01.
           MOVE VDUNUM0 TO DL-DISC-FRMLA.
           IF NOT DL-DISC-FRMLA-VALID
              MOVE "ENTER 0, 1, 2, 3, 4 OR 5 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO DL-DISC-CODE.
           IF NOT DL-DISC-CODE-VALID
              MOVE "ENTER ' ' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM0 TO DL-DISC-TERM(1).
           GO TO REC-RATE.
       A-04.
           MOVE VDUNUM0 TO DL-DISC-TERM(2).
           GO TO REC-RATE.
       A-05.
           MOVE VDUNUM0 TO DL-DISC-TERM(3).
           GO TO REC-RATE.
       A-06.
           MOVE VDUNUM0 TO DL-DISC-TERM(4).
           GO TO REC-RATE.
       A-07.
           MOVE VDUNUM0 TO DL-DISC-TERM(5).
           GO TO REC-RATE.
       A-08.
           MOVE VDUNUM0 TO DL-DISC-TERM(6).
           GO TO REC-RATE.
       A-09.
           MOVE VDUNUM0 TO DL-SVACCT-FRMLA.
           IF NOT DL-SVACCT-FRMLA-VALID
              MOVE "ENTER 0 OR 1 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUBUF TO DL-SVCODE.
           IF NOT DL-SVCODE-VALID
              MOVE "ENTER SPACE ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM2 TO DL-SVACCT-RATE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO DL-SVACCT-AMT.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM0 TO DL-SVACCT-LCHGPER.
           IF DL-SVACCT-LCHGPER > 100
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM0 TO DL-SVACCT-INTPER.
           IF DL-SVACCT-INTPER > 100
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM0 TO DL-SVACCT-DEFPER.
           IF DL-SVACCT-DEFPER > 100
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       REC-RATE.
           MOVE "EN N024" TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO REC-RATE.
           COMPUTE SUB = ELE - 2.
           MOVE VDUNUM4 TO DL-DISC-RATE(SUB).
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
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
           MOVE CHANGED-DL-REC TO DL-REC.
           PERFORM REWRITE-DL1-FILE.
       END-WRITE-MODULE.
           PERFORM CLOSE-DL1-FILE.

      ******************************
       DISPLAY-FIELDS SECTION.
      ******************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE DL-NAME TO DLNAME.
           MOVE DL-DISC-FRMLA TO A01.
           MOVE DL-DISC-CODE TO A02.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE DL-DISC-TERM(SUB) TO ADISCTERM(SUB)
              MOVE DL-DISC-RATE(SUB) TO ADISCRATE(SUB)
           END-PERFORM.

           MOVE DL-SVACCT-FRMLA   TO A09.
           MOVE DL-SVCODE         TO A10.
           MOVE DL-SVACCT-RATE    TO A11.
           MOVE DL-SVACCT-AMT     TO A12.
           MOVE DL-SVACCT-LCHGPER TO A13.
           MOVE DL-SVACCT-INTPER  TO A14.
           MOVE DL-SVACCT-DEFPER  TO A15.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO DLMAN5-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLMAN5-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DLMAN5-SCREEN UPON DLMAIN-WINDOW-HANDLE.
           MOVE DLMAN5-FORM-FIELDS TO WR-BUF.
           MOVE DLMAN5-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE DLMAN5-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLMAN5-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/DLMAN5_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
       COPY "LIBGB/ACCESS.CPY".
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

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-DL2-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDL1IN.CPY".
       COPY "LIBLP/LPDL2RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLMAN5-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE DLMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
