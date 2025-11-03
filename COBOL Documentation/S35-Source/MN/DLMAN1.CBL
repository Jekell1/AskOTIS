       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLMAN1.
      *************************************************************************
      *                 (MN)
      *  DESCRIPTION:   DEALER MASTER FILE MAINTENANCE
      *
      * REV:
      *  SLC 040489 ADDED ANOTHER PAGE. NOW = 4.
      *  JTG 060292 ADDED LOGIC FOR DL-LC-CODE (ROSE SHANIS)
      *  MJD ADDED DL-AFP-DEALER
      *  030797 BAH ADDED DL-RECOURSE-FRMLA (FAIRCO, FL), FROM A50
      *  BLV 031897 ADD F2 DEALER MEMOS
      *  MJD 061897 ADDED TEST OF ACTIVE BALANCES IN DSFILE BEFORE
      *             ALLOWING DELETE. DO NOT ALLOW DELETE IF DL-TERMDATE
      *             IS NON-ZERO AND IS GREATER THAN TODAYS DATE.
      *  KEC 111198 ADDED DL-REVOLVING; REGENCY REVOLVING
      *  CS  111698 ADDED DL-FAX-NUMBER
      *  MJD 990702 ADDED DL-LPO FOR TFC REQ#31
      *  MJD 990722 FIXED AFP-DEALER/CLASS TEST TO COUNT " " AS "N".
      *  MG  990909 DELETED AFP-DEALER
      *  BAH 011115 ADDED PASSWORD FOR "DELETE", TFC PR# 1644
      *  BLV 020108 ADDED CLEAR OF DL-BANK-ROUTING-NO (LPDLCL) REGACC PR# 1691
      *  BAH 050816 CHANGED SCREEN DISPLAY ONLY LENGTH OF BOTH ADDRESSES
      *             FROM 25 TO 24 FOR CHECK WRITING ! NEWPORT PR# 2589
      *  BAH 140310 ADDED SET OF DL-TRACK-CHANGE-FLAG, REGACC PR# 350
      *  BAH 140911 FIXED PHONE ENTRY
      * KEC 2016.0309 CHANGED CD-TYPE OF "LO" TO USE NEW LAYOUT (CD-LO-REC).
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * KEC 2016.0329 CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      * BLM 160513 FIX FOR NEW DS-DATE FORMAT
      * KEC 2016.1221 CHANGED FIELD WORKERS TO CONTAIN FILE PREFIXES (DL).
      * KEC 2017.0105 CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *  CS 170420 ACTIVATE 8 DIGIT DATES, DL-ORIGDATE,DL-TERMDATE
      * BAH 181011 GLOBAL DLFILE
      * BAH 181015 GLOBAL DMFILE
      * BAH 190129 SPLIT CDFILE
      * BAH 190218 REMOVED DM-DL-NUM, USE DM-DL-DLNO TO BE CONSISTENT
      *  CS 190423 REMOVE DS APP STATS
      * BAH 20190628 REMOVED DL-REVOLVING
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
           "@(#)MN/DLMAN1.CBL S35 02/21/24-12:59:58 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DM.CPY".
       COPY "LIBLP/LP01DM_SQL.CPY".
       COPY "LIBLP/LPWSDM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  SUB             PIC 99      VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
           88 INVALID-ELE VALUE 9.
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
       01  DS-OK           PIC X       VALUE " ".
       01  FIRST-SCAN      PIC 9       VALUE 0.

      * THIS IS NEEDED FOR CLEAR-DL-FILE
       01  HOLD-DL1-KEY.
           03  HOLD-DL-BRNO            PIC 9(4).                 
           03  HOLD-DL-NUM             PIC 9(6).
           03  HOLD-DL-DLNO            REDEFINES HOLD-DL-NUM
                                       PIC 9(4)V99.

       01  HOLD-DS-DATE-CCYYMM    PIC 9(8).

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
           03  A02         PIC X(24).
           03  A03         PIC X(24).
           03  A04         PIC X(15).
           03  A05         PIC X(2).
           03  A06         PIC X(10).
           03  A07         PIC X(12).
           03  A071        PIC X(12).
           03  A08         PIC X(16).
           03  A09         PIC X(4).
           03  A10         PIC X(3).
           03  A11         PIC 99/99/99.
           03  A12         PIC 99/99/99.
           03  A13         PIC X(3).
           03  A14         PIC X.
           03  A15         PIC X.
           03  A16         PIC ZZZZZZZ.ZZ-.
           03  A17         PIC ZZZZZZZ.ZZ-.
           03  A18         PIC ZZZZZZZ.ZZ-.
           03  A19         PIC ZZZZZZZZZ.ZZ-.
           03  A20         PIC ZZZZZZZZZ.ZZ-.
           03  A21         PIC ZZZZZZZZZ.ZZ-.
           03  A22         PIC X(04).
           03  A23         PIC ZZ.

       01  DISPLAY-LIST.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(24) VALUE "060 070 071 080 090 100 ".
           03  FILLER PIC X(20) VALUE "110 120 130 140 150 ".
           03  FILLER PIC X(20) VALUE "160 170 180 190 200 ".
           03  FILLER PIC X(12) VALUE "210 220 230.".

       78  MAX                  VALUE 23.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      * 1. DEALER NAME
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      * 2. ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 3. ADDRESS 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 4. CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 5. STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6. ZIP
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 7. PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 8. ABBREV. NAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 9. N/A  ****** NOT IN USE***************
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *10. PURPOSE CODE
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *11. ORIG-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *12. TERM-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *13. MFG CODE
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *14. RECOURSE FORMULA
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *15. N/A
           03  FILLER      PIC X(8) VALUE "E  A010N".
      *16 - 18. RECOURSE CEILINGS
           03  FILLER      PIC X(8) VALUE "ENNS072 ".
           03  FILLER      PIC X(8) VALUE "ENNS072 ".
           03  FILLER      PIC X(8) VALUE "ENNS072 ".
      *19 - 21. RECOURSE BALANCES
           03  FILLER      PIC X(8) VALUE "ENNS092 ".
           03  FILLER      PIC X(8) VALUE "ENNS092 ".
           03  FILLER      PIC X(8) VALUE "ENNS092 ".
      *22. LPO CODE
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *23. LC CODE
           03  FILLER      PIC X(8) VALUE "ENNN020 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS MAX TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       01  HOLD-SYS-DATE         PIC 9(8)   COMP-3.

       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  DELETE-SEQ              VALUE 1.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/ELSCANW.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/DLMAN1_DEF.CPY".
       COPY "LIBMN/DLMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/DLMAN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************
       MAIN-PROGRAM SECTION.
      **********************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DLMAN1" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO DLMAIN-LINK.

           PERFORM GETDAT.
           MOVE SYS-DATE TO HOLD-SYS-DATE.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "BRANCH RECORD MISSING" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           PERFORM GET-GPENV.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           IF GP-ENTER-LPO-FG = "Y"
              MOVE " " TO CHFG(22)
           ELSE
              MOVE "N" TO CHFG(22).
           IF BR-LC-CODE = "Y"
              MOVE "S  A26023P:S." TO VDU
              MOVE "23. LATE CHARGE CODE     :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020230." TO VDU
              MOVE "__" TO VDUBUF
              PERFORM SCREENIO
              MOVE " " TO CHFG(23)
           ELSE
              MOVE "N" TO CHFG(23).

           PERFORM REC-DL-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 2 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE DL-NUM TO SAVEKEY                   
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

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

           MOVE DL-PATH-OWNBR TO HOLD-DL-BRNO.              
           MOVE DL-NUM        TO HOLD-DL-NUM.
       READ-DLFILE.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 0
              MOVE DL-REC TO ORIGINAL-DL-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-DL-MODULE.
           PERFORM NEW-RECORD-SETUP.
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
           IF INVALID-ELE
              GO TO CREATE-ROUTINE.

      *    PERFORM CHECK-RESERVE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              MOVE "D"              TO MEMOSC-ONLY
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

           IF ACTION = "DELETE"
              MOVE DELETE-SEQ TO PASSWDW-SEQ
              MOVE "TO DELETE THIS DEALER..." TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 2 AND NOT = 3 AND NOT = 4
                                      AND NOT = 5 AND NOT = 6
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
      *    PERFORM CHECK-RESERVE.
 
           IF INVALID-ELE
              GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF ELE = 23
              IF BR-LC-CODE NOT = "Y"
                 GO TO CREATE-ROUTINE.
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
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF ELE = 22
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    GO TO GOTO-FIELD1
                 ELSE
                    GO TO ENTER-ELE
                 END-IF
              IF ELE = 23
                 PERFORM CODE-SCAN-EL
                 IF IO-FG = 0
                    GO TO GOTO-FIELD1
                 ELSE
                    GO TO ENTER-ELE.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD1.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *********************************
      *     SET RECORD ELEMENTS       *
      *********************************
       A-01.
           MOVE VDUBUF TO DL-NAME.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO DL-ADR1.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO DL-ADR2.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO DL-CITY.
           GO TO CREATE-ROUTINE.
       A-05.
           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST1-KEY.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           MOVE VDUBUF TO DL-STATE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO DL-ZIP.
           GO TO CREATE-ROUTINE.
       A-07.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO DL-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120070." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.

       A-071.
           MOVE "R NA120071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-071.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO A-071.
           MOVE TEL-BUF TO DL-FAX-NUMBER.
           PERFORM SEND-PHONE.
           MOVE "S  A120071." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO DL-SHORT.
           IF DL-SHORT = SPACES
              MOVE "S  A160080." TO VDU
              MOVE DL-NAME TO VDUBUF DL-SHORT
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           GO TO CREATE-ROUTINE.
       A-09.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUBUF TO DL-PURPOSE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDU-DATE-YYYYMMDD TO DL-ORIGDATE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDU-DATE-YYYYMMDD TO DL-TERMDATE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO DL-MFGCODE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUBUF TO DL-RECOURSE-FRMLA.
           IF NOT (DL-RECOURSE-FRMLA = " " OR "A")
              MOVE "ENTER ' ' OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-15.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM2 TO DL-FULLCEIL.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM2 TO DL-PARTCEIL.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM2 TO DL-NONCEIL.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO DL-FULLCEIL-BAL.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM2 TO DL-PARTCEIL-BAL.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM2 TO DL-NONCEIL-BAL.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUBUF TO DL-LPO.
           PERFORM VERIFY-LPO-CODE.
           IF IO-FG NOT = 0 AND DL-LPO NOT = SPACES
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM0 TO DL-LC-CODE.
           PERFORM VERIFY-EL-CODE.
           IF IO-FG NOT = 0 AND DL-LC-CODE NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ***********************************
       CODE-SCAN SECTION.
      ***********************************
           MOVE 15 TO CDSCAN-BEGIN-X.
           MOVE 3 TO CDSCAN-BEGIN-Y.

      * LOAN PROCESSING OFFICE, FIELD 22
           MOVE "LO"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A040220." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************************************
       CODE-SCAN-EL SECTION.
      ******************************************************************

      * EXTENDED LATE CHARGE CODE, FIELD 23

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.
           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00") OR (ELSCAN-CODE = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN-EL.

           MOVE "SNNN020230." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ZEROES        TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN-EL.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "DLMAN1"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       EXIT-VERIFY-PASSWORD.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
       REWRITE-RECORD.
           PERFORM OPEN-DL1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
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

           IF DL-NAME NOT = DLOR-NAME OR
               DL-ADR1 NOT = DLOR-ADR1 OR
                DL-ADR2 NOT = DLOR-ADR2 OR
                 DL-CITY NOT = DLOR-CITY OR
                  DL-STATE NOT = DLOR-STATE OR
                   DL-ZIP NOT = DLOR-ZIP OR
                    DL-PHONE NOT = DLOR-PHONE OR
                     DL-FAX-NUMBER NOT = DLOR-FAX-NUMBER 
              MOVE "Y" TO DL-TRACK-CHANGE-FLAG.
           PERFORM REWRITE-DL1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-DL1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.
       END-WRITE-MODULE.
           PERFORM CLOSE-DL1-FILE.

      *****************************
       DELETE-MODULE SECTION.
      *****************************

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF DL-REC NOT = ORIGINAL-DL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           IF DL-TERMDATE NOT = 0
              IF HOLD-SYS-DATE < DL-TERMDATE
                 MOVE "CANNOT DELETE PRIOR TO TERMINATION DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-DELETE-MODULE.

           PERFORM TEST-FOR-OUTSTANDING-ACCT.
           IF DS-OK NOT = " "
              MOVE "THERE ARE OUTSTANDING BALANCES FOR THIS DEALER"
                                            TO MESS
              PERFORM SEND-MESS
              PERFORM NO-DELETE
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-DL1-FILE.

           PERFORM OPEN-DM1-FILE.

           MOVE DL-BRNO             TO DM-BRNO
                                       QDM1-WBEG-BRNO
                                       QDM1-WEND-BRNO.
           MOVE DL-DLNO             TO DM-DL-DLNO
                                       QDM1-WBEG-DLNO
                                       QDM1-WEND-DLNO.
           MOVE 0                   TO DM-DATE DM-SEQ
                                       QDM1-WBEG-DATE-REV
                                       QDM1-WBEG-SEQ.
           MOVE ALL "9"              TO QDM1-WEND-DATE-REV.
           MOVE ALL "9"              TO QDM1-WEND-SEQ.

           PERFORM START-DM1-FILE.
       DELETE-DL-MEMOS.
           PERFORM READ-DM1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (DM-BRNO NOT = DL-BRNO)
              GO TO END-DELETE-MODULE.

           IF DL-DLNO NOT = DM-DL-DLNO
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-DM1-FILE.

           GO TO DELETE-DL-MEMOS.
       END-DELETE-MODULE.
           PERFORM CLOSE-DM1-FILE.
           PERFORM CLOSE-DL1-FILE.

      ******************************************************************
       TEST-FOR-OUTSTANDING-ACCT SECTION.
      ******************************************************************

      * TEST DEALER FOR OUTSTANDING BALANCES BEFORE ALLOWING DELETE
      *
      * IF ANY ONE STATE RECORD HAS ACTIVE BALANCES, THEN DO NOT ALLOW DELETE

           MOVE " " TO DS-OK.

           PERFORM OPEN-DS1-FILE.

           MOVE HOLD-SYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM HOLD-DS-DATE-CCYYMM.
           MOVE DL-BRNO     TO DS-BRANCH.
           MOVE DL-NUM      TO DS-NUM.
           MOVE 0           TO DS-CLASS.

           PERFORM START-DS1-FILE.
       TEST-NEXT-DS.
           PERFORM READ-DS1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (DS-BRANCH NOT = DS-PATH-OWNBR)
              GO TO EXIT-TEST-FOR-OUTSTANDING-ACCT.

           IF DS-DATE-CCYYMM > HOLD-DS-DATE-CCYYMM OR
               DS-BRANCH > DL-BRNO OR
                 DS-NUM > DL-NUM
              GO TO EXIT-TEST-FOR-OUTSTANDING-ACCT.

      * RESERVE FROM PRINCIPLE
            IF DS-DLRVPRIN(1) NOT = 0
               OR DS-DLRVPRIN(2) NOT = 0
               OR DS-DLRVPRIN(3) NOT = 0
      * RESERVE FROM CHARGES
               OR DS-DLRVCHGS(1) NOT = 0
               OR DS-DLRVCHGS(2) NOT = 0
               OR DS-DLRVCHGS(3) NOT = 0
      * RESERVE PART HELD
               OR DS-DLPARVHELD NOT = 0
      * UNPAID CHECKS
               OR DS-UNPAIDCK NOT = 0
      * SUM WEIGHTED AVERAGES
               OR DS-SUMWGTAV NOT = 0
               OR DS-SUMWGTTM NOT = 0
      * RECOURSE OUTSTANDING
               OR DS-RCOURSE-UNITS(1) NOT = 0
               OR DS-RCOURSE-AMT(1) NOT = 0
               OR DS-RCOURSE-UNITS(2) NOT = 0
               OR DS-RCOURSE-AMT(2) NOT = 0
               OR DS-RCOURSE-UNITS(3) NOT = 0
               OR DS-RCOURSE-AMT(3) NOT = 0
      * OUTSTANDING
               OR DS-OUTSAMT(1) NOT = 0
               OR DS-OUTSUNIT(1) NOT = 0
      * P&L
               OR DS-PLOSAMT(1) NOT = 0
               OR DS-PLOSUNIT(1) NOT = 0
      * P&L RECV
               OR DS-RCPLAMT(1) NOT = 0
               OR DS-RCPLUNIT(1) NOT = 0
      * JUDGEMENT
               OR DS-JDOSAMT(1) NOT = 0
               OR DS-JDOSUNIT(1) NOT = 0
      * REPO ON HAND
               OR DS-AMT-REPO(1 1 1) NOT = 0
               OR DS-NO-REPO(1 1 1) NOT = 0
      * REPO REDEEMED
               OR DS-AMT-REPO(1 1 2) NOT = 0
               OR DS-NO-REPO(1 1 2) NOT = 0
      * REPO SOLD LIVE RECEIVABLE
               OR DS-AMT-REPO(1 1 3) NOT = 0
               OR DS-NO-REPO(1 1 3) NOT = 0
      * REPO SOLD P&L ACTIVE
               OR DS-AMT-REPO(1 1 4) NOT = 0
               OR DS-NO-REPO(1 1 4) NOT = 0
      * REPO SOLD P&L INACTIVE
               OR DS-AMT-REPO(1 1 5) NOT = 0
               OR DS-NO-REPO(1 1 5) NOT = 0
      * APPLICATIONS PEND/COND/APPVD
      *        OR DS-NO-PENDING(1 1 1) NOT = 0
      *        OR DS-NO-COND-APP(1 1 1) NOT = 0
      *        OR DS-NO-APPROVED(1 1 1) NOT = 0
      *        OR DS-NO-PENDING(1 1 2) NOT = 0
      *        OR DS-NO-COND-APP(1 1 2) NOT = 0
      *        OR DS-NO-APPROVED(1 1 2) NOT = 0
                   MOVE "N" TO DS-OK
                   GO TO EXIT-TEST-FOR-OUTSTANDING-ACCT.

           GO TO TEST-NEXT-DS.

       EXIT-TEST-FOR-OUTSTANDING-ACCT.
           PERFORM CLOSE-DS1-FILE.


      ********************************
       NEW-RECORD-SETUP SECTION.
      ********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.

           PERFORM CLEAR-DL-FILE.

           MOVE 0 TO ELE.
           MOVE "SN N040090." TO VDU.
           MOVE BR-NO TO VDUNUM0 DL-BRNO.       
           PERFORM SCREENIO.
           MOVE "S  A020050." TO VDU.
           MOVE BR-STATE TO VDUBUF DL-STATE.
           PERFORM SCREENIO.

           MOVE "SN S072160." TO VDU.
           MOVE 9999999.99 TO VDUNUM2 DL-FULLCEIL.
           PERFORM SCREENIO.
           MOVE "SN S072170." TO VDU.
           MOVE 9999999.99 TO VDUNUM2 DL-PARTCEIL.
           PERFORM SCREENIO.
           MOVE "SN S072180." TO VDU.
           MOVE 9999999.99 TO VDUNUM2 DL-NONCEIL.
           PERFORM SCREENIO.

           MOVE "Y" TO DL-TRACK-CHANGE-FLAG.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************
       DISPLAY-FIELDS SECTION.
      ******************************

           MOVE SPACES TO DISPLAY-BUF.
           MOVE DL-NAME TO A01.
           MOVE DL-ADR1 TO A02.
           MOVE DL-ADR2 TO A03.
           MOVE DL-CITY TO A04.
           MOVE DL-STATE TO A05.
           MOVE DL-ZIP TO A06.

           IF DL-PHONE NOT = 0
              MOVE DL-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO A07.

           IF DL-FAX-NUMBER NOT = 0
              MOVE DL-FAX-NUMBER TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO A071.

           MOVE DL-SHORT   TO A08.
           MOVE DL-BRNO    TO A09.
           MOVE DL-PURPOSE TO A10.
           IF DL-ORIGDATE NOT = 0
              MOVE DL-ORIGDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO A11.

           IF DL-TERMDATE NOT = 0
              MOVE DL-TERMDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO A12.

           MOVE DL-MFGCODE        TO A13.
           MOVE DL-RECOURSE-FRMLA TO A14.
           MOVE SPACES            TO A15.
           MOVE DL-FULLCEIL       TO A16.
           MOVE DL-PARTCEIL       TO A17.
           MOVE DL-NONCEIL        TO A18.
           MOVE DL-FULLCEIL-BAL   TO A19.
           MOVE DL-PARTCEIL-BAL   TO A20.
           MOVE DL-NONCEIL-BAL    TO A21.
           MOVE DL-LPO TO A22.
           IF BR-LC-CODE = "Y"
              MOVE DL-LC-CODE TO A23.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPDLCL.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO DLMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLMAN1-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DLMAN1-SCREEN UPON DLMAIN-WINDOW-HANDLE.
           MOVE DLMAN1-FORM-FIELDS TO WR-BUF. 
           MOVE DLMAN1-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.


      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE DLMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLMAN1-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/DLMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/GETDAT.CPY".
       NO-DELETE.
           MOVE "STATISTICS RECORDS EXIST - UNABLE TO DELETE!" TO MESS.
           PERFORM SEND-MESS.
       VERIFY-LPO-CODE.
           PERFORM OPEN-CD1-FILE.
           MOVE "LO"   TO CD-TYPE.
           MOVE DL-LPO TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       VERIFY-EL-CODE.
           PERFORM OPEN-CD1-FILE.
           MOVE "EL"       TO CD-TYPE.
           MOVE DL-LC-CODE TO CD-EL-CODE.
           MOVE SPACES     TO CD-EL-FILLER.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
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
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       INVALID-FLAG.
           MOVE "MUST ENTER '0', '1', OR '2'" TO MESS.
           PERFORM SEND-MESS.

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DM1-FILE.
           PERFORM CLOSE-DL2-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDMGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPDL1IN.CPY".
       COPY "LIBLP/LPDL2RN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBLP/LPDM1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLMAN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE DLMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
