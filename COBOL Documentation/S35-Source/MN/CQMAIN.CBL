       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CQMAIN.
       DATE-WRITTEN. 012786.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   COLLECTOR QUEUE FILE MAINTENANCE
      *
      * REV.
      * KEC 020796 ADDED CQ-DATE.
      * KEC 020896 MOVED "D" TO CQ-TYPE WHEN CREATING NEW RECORD.
      * KEC 031496 REMOVED REFERENCES TO CQ-BRANCH AND CQ-SEQNO.
      * BLL 031496 FIX CQ DATE ROUTINES
      * KEC 042496 ADDED CQ-IN-PROCESS.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * BLV 000908 CHANGED FIELD 4 (TIME) TO BE SIGNED, MAY BE SIGNED
      *            TIME ZONE FACTOR, TFC PR# 25
      * BLV 000912 ADD FIELD 12, CQ-TIME-ZONE-ERROR, TFC PR# 25
      * BLV 040615 ADD FIELD 2, CQ-REV-BEHAVIOR-RISK, REGACC PR#59
      * BLM 110323 CORRECTED SIZE OF ORIGINAL-REC & CHANGED-REC, CHANGED
      *            FROM 49 BYTES TO 52 BYTES, WORLD
      * MJD 131210 A30 CONVERSION COMPLETED
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      * KEC 2016.1004 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED ORIGINAL-REC & CHANGED-REC TO BE
      *         ORIGINAL-CQ-REC & CHANGED-CQ-REC USING CQ-SIZE.
      *
      * KEC 2016.1005 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED SETTING CQ-BRNO.
      *
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING LIBSP/SPSETCQ.
      *         ADDED LOGIC OF CONVERTING & SETTING CQ-DATE (CCYYMMDD).
      *         REMOVED USING LN-SEQNO & LN-MORELN.
      *  CS 190130 GLOBAL CQFILE, STILL HAVE ITEM WITH MOVE VDUBUF TO CQ2-KEY
      *            THE SCAN INPUT IS 14 CHARACTERS LONG
      *  CS 190603 CLEAR FUTURE FIELDS ON NEW RECORD
      *  BAH 20190723 ADDED CLEAR-CQ-FILE, SPCQCL
      *   CS 191115 CHANGED SCAN TO READ ON CQ1-FILE WHICH MATCHES THE 
      *             SCREEN INSTEAD OF CQ2 
      *   CS 20200214 CHANGED SCANKEY ENTRY TO 8 (WAS 8 IN A15 INCL SEQ)
      *               ENTRY FROM ALPHA TO NUMERIC
      *   CS 20200506 REMOVED HOLD-CQ-FUTURE-KEY1,KEY2
      *  BLM 20210723 MOVE CQ-ACCTNO TO LN-ACCTNO IN  ENTER-SCANKEY-DISPLAY
      *
      * JKC 2025-0424 S35Q-200 ADDED NEW LOGIC FOR UPDATE THE VCQFILE;
      *          THE ORIGINAL VCQFILE WAS CREATED ONE PER USER PUT INTO
      *          /USR/TMP; THE NEW VCQFILE IS CREATED FOR ONE PER BRANCH
      *          PUT INTO /USR/DATA/R#/WK/VCQ#### WHICH WAS DONE FOR
      *          POTENTIALLY AVOID ISSUES WITH A VCQFILE ONE PER USER.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBSP/SPFSVCQ.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBSP/SPFDVCQ.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/CQMAIN.CBL J35 04/24/25-15:03:01 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SPWSVCQ.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ORIGINAL-CQ-REC                     PIC X(CQ-SIZE).
       01  CHANGED-CQ-REC                      PIC X(CQ-SIZE).

      *-----------------------------------------------------------------
       01  ERRCD                               PIC X(1)  VALUE SPACES.
       01  ONE                                 PIC S9.
       01  ELE                                 PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY                           PIC X(4).
           03  CHNO                            PIC 9(2).
       01  IO-TYPE                             PIC X(3).
       01  NEW-FLAG                            PIC 9.
       01  FIRST-SCAN                          PIC 9     VALUE 0.
       01  ENTER-SEQNO                         PIC X.
       01  BR-ALLOW-WINDOWS                    PIC X VALUE "Y".

       01  HOLD-KEY.
           03  HOLD-ACCTNO                     PIC 9(8).

       01  SHOW-BRNO-ACCT-COLLID               PIC X(17). 

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC XXX.
           03  F011          PIC X(25).
           03  F02           PIC 9.
           03  F03           PIC X.
           03  F04           PIC 99/99/99.
           03  F05           PIC 9999-.
           03  F06           PIC 99999.
           03  F07           PIC 99999.
           03  F08           PIC ZZZ,ZZ9.
           03  F09           PIC X.
           03  F10           PIC XXX.
           03  F101          PIC X(25).
           03  F11           PIC X.
           03  F12           PIC 99/99/99.
           03  F13           PIC X.

       01  DISPLAY-LIST.
           03  FILLER        PIC X(32) VALUE
           "010 011 020 030 040 050 060 070 ".
           03  FILLER        PIC X(28) VALUE
           "080 090 100 101 110 120 130.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX               PIC 9(2)    VALUE 13.
       01  SPEC-TABLE.
      * 01. COLLECTOR ID
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * 02. BEHAVIOR RISK FACTOR  
           03  FILLER        PIC X(8) VALUE "ENNN010 ".
      * 03. SEQ FG  
           03  FILLER        PIC X(8) VALUE "E  A010 ".
      * 04. DATE  
           03  FILLER        PIC X(8) VALUE "ERNM060 ".
      * 05. START TIME  
           03  FILLER        PIC X(8) VALUE "ENNS040 ".
      * 06. REV. DELINQUENCY  
           03  FILLER        PIC X(8) VALUE "ENNN050 ".
      * 07. REV. PAST DUE DAYS  
           03  FILLER        PIC X(8) VALUE "ENNN050 ".
      * 08. REV. BALANCE  
           03  FILLER        PIC X(8) VALUE "E NN060 ".
      * 09. WORKED FLAG 
           03  FILLER        PIC X(8) VALUE "E  A010 ".
      * 10. WORK COLLECTOR  
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * 11. WORK IN PROCESS 
           03  FILLER        PIC X(8) VALUE "E  A010 ".
      * 12. DATE  QUEUED  
           03  FILLER        PIC X(8) VALUE "ERNM060 ".
      * 13. TIME ZONE ERROR FLAG 
           03  FILLER        PIC X(8) VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 13 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/VCQUPD_LNK.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01 CQMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CQMAIN_DEF.CPY".
       COPY "LIBMN/CQMAIN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CQMAIN_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON VCQ-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE VCQ-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "CQMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CQMAIN-WINDOW-HANDLE.

       MAIN-MODULE.

           PERFORM REC-CQ-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-CQ-MODULE SECTION.
      ******************************************************************
       REC-CQ-MODULE-NO.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE LN-REC.
           PERFORM FORM-RESET.

           MOVE "Y" TO ENTER-SEQNO.

           IF BR-ALLOW-WINDOWS = "Y"
              MOVE "F7-EXIT, ENTER ACCOUNT NUMBER:" TO MESS
           ELSE
              MOVE "F7-EXIT, ENTER ACCOUNT NUMBER:" TO MESS.

           PERFORM SEND-LEGEND.
           MOVE "EN N080LNNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-CQ-MODULE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CQ-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-CQ-MODULE-NO.

           MOVE VDUNUM0 TO LN-ACCTNO.

      *-----------------------------------------------------------------
       READ-LOAN-FILE.

           PERFORM GET-LN.

           IF IO-FG = 9
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO REC-CQ-MODULE-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-CQ-MODULE-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-CQ-MODULE-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.

           MOVE "SN N080LNNO." TO VDU.
           MOVE LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

       READ-CQFILE.

           MOVE LN-ACCTNO TO CQ-ACCTNO.
           PERFORM OPEN-CQ1-FILE.
           PERFORM READ-CQ1-FILE.
           PERFORM CLOSE-CQ1-FILE.

           IF ( IO-FG = 0 )
                 MOVE CQ-REC TO ORIGINAL-CQ-REC
                 PERFORM DISPLAY-FIELDS
           ELSE
              MOVE LN-ACCTNO TO HOLD-ACCTNO
              PERFORM NEW-RECORD-SETUP.

       END-REC-CQ-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "F1-RESET, F6-CONT SCAN, ENTER KEY:" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-CQ1-FILE.
           MOVE CQ-PATH-OWNBR TO CQ-BRNO.
           MOVE 0             TO CQ-ACCTNO.


           PERFORM START-CQ1-FILE.

           MOVE 0 TO FIRST-SCAN.

       ENTER-SCANKEY-KEY.

           MOVE "E  N080SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

           IF VDUSTATUS = "1<"
              GO TO ENTER-SCANKEY-READ-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY-KEY.

      *    IF VDUBUF = SPACES
      *       IF FIRST-SCAN NOT = 0
      *          GO TO ENTER-SCANKEY-CLOSE
      *       ELSE
      *          GO TO ENTER-SCANKEY-KEY.


           IF VDUNUM0 = ZEROS 
              IF FIRST-SCAN NOT = 0
                 GO TO ENTER-SCANKEY-CLOSE
              ELSE
                 GO TO ENTER-SCANKEY-KEY.
 
           MOVE VDUNUM0  TO CQ-ACCTNO.

           PERFORM START-CQ1-FILE.

       ENTER-SCANKEY-READ-NEXT.

           IF FIRST-SCAN = 0
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-CQ1-FILE-NEXT.
           IF IO-FG = 9 OR (CQ-BRNO NOT = CQ-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

      *    UNLOCK CQ-FILE.    

           MOVE CQ-REC TO ORIGINAL-CQ-REC.
           PERFORM ENTER-SCANKEY-DISPLAY.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-SCANKEY-KEY.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-DISPLAY.

           MOVE CQ-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.
           MOVE "SN N080LNNO." TO VDU.
           MOVE CQ-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A020LNSEQ." TO VDU.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-CLOSE.

           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-END.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
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

           MOVE "F1, 'DELETE', ENTER FIELD #:" TO MESS.
           PERFORM SEND-LEGEND.
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

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
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
       
       TEST-F6.
           IF ( VDUSTATUS = "1<" ) AND ( ELE = 1 OR 10 )
              IF ( BR-ALLOW-WINDOWS = "Y" )
                 PERFORM CODE-SCAN
                 IF ( IO-FG NOT = 0 )
                    GO TO ENTER-ELE
                 ELSE
                    MOVE CDBSCN-CODE TO VDUBUF
                    MOVE "00"        TO VDUSTATUS.
       
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09
                 F-10 F-11 F-12 F-13
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUBUF     TO CQ-COLLID CDB-CODE.
           MOVE "CN"               TO CDB-TYPE.
           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.
           PERFORM VERIFY-CD.
           IF IO-FG NOT = 0
              MOVE "NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE "S  A250011." TO VDU.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       F-02.
           IF NOT (VDUNUM0 = 0 OR 1 OR 2 OR 3 OR
                             4 OR 5 OR 6 OR 7 OR
                                       8 OR 9)
              MOVE "ENTER A VALUE FROM 0 - 9" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           COMPUTE CQ-REV-BEHAVIOR-RISK = 10 - VDUNUM0.

           GO TO CREATE-ROUTINE.
       
       F-03.
           MOVE VDUBUF TO CQ-SEQ-FG.
           IF NOT (CQ-SEQ-FG = "N" OR "B" OR "R")
              MOVE "ENTER 'B' OR 'N' OR 'R':" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDU-DATE-YYYYMMDD TO CQ-DATE.
           GO TO CREATE-ROUTINE.

       F-05.
           MOVE VDUNUM0 TO CQ-START-TIME.
           GO TO CREATE-ROUTINE.
       
       F-06.
           MOVE VDUNUM0 TO CQ-REV-DEL.
           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDUNUM0 TO CQ-REV-PDUE-DAYS.
           GO TO CREATE-ROUTINE.

       F-08.
           MOVE VDUNUM0 TO CQ-REV-BAL.
           GO TO CREATE-ROUTINE.

       F-09.
           MOVE VDUBUF TO CQ-WORKED-FG.

           IF NOT (CQ-WORKED-FG = "Y" OR "N" OR "R" OR " ")
              MOVE "ENTER 'Y' OR 'N' OR SPACE:" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( CQ-WORKED-FG = "N" OR " " )
              MOVE "S  A030100." TO VDU
              MOVE " " TO VDUBUF CQ-WORK-COLLID
              PERFORM SCREENIO
              MOVE "S  A300101." TO VDU
              PERFORM SCREENIO
              MOVE "N" TO CHFG(10)
           ELSE
              MOVE " " TO CHFG(10).

           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDUBUF     TO CQ-WORK-COLLID CDB-CODE.
           MOVE "CN"               TO CDB-TYPE.
           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.
           PERFORM VERIFY-CD.
           IF ( IO-FG = 9 )
              MOVE "NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE "S  A025101." TO VDU.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       
       F-11.

           IF ( VDUBUF = "Y" OR " " )
              MOVE VDUBUF TO CQ-IN-PROCESS
           ELSE
              MOVE "ENTER 'Y' OR SPACE ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDU-DATE-YYYYMMDD TO CQ-DATE-QUEUED.
           GO TO CREATE-ROUTINE.

       F-13.

           IF VDUBUF = "Y" OR "N"
              MOVE VDUBUF TO CQ-TIME-ZONE-ERROR
           ELSE
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           MOVE EXT-FILPATH-BASE    TO VCQ-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO VCQ-ALL-MACHINE.
           MOVE EXT-FILPATH-BRNO    TO VCQ-ALL-BRNO.
           MOVE VCQ-ALL-PATH        TO VCQ-PATH.

           PERFORM OPEN-CQ1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CQ-REC TO CHANGED-CQ-REC.

           IF ORIGINAL-CQ-REC = CHANGED-CQ-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-CQ1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF CQ-REC NOT = ORIGINAL-CQ-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-CQ-REC TO CQ-REC.
           PERFORM REWRITE-CQ1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  UPDATE VCQ-FILE FOR UPDATE TO CQ-FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SHOW-BRNO-ACCT-COLLID.
           STRING CQ-BRNO, "-",
                  CQ-ACCTNO, "-",
                  CQ-COLLID
                  DELIMITED BY SIZE INTO SHOW-BRNO-ACCT-COLLID.

           MOVE CQ-PATH-OWNBR   TO VCQ-BRNO.
           MOVE CQ-ACCTNO       TO VCQ-ACCTNO.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.

           IF ( IO-BAD )
              DISPLAY MESSAGE BOX
                      "ERROR: QUEUE RECORD NOT FOUND [1]", X"0A"
                      " ", X"0A"
                      "COLLECTOR ACCOUNT: ",
                       SHOW-BRNO-ACCT-COLLID, X"0A"
                      "STOP!!! CALL BRANCH SUPPORT IMMEDIATELY!!!"
                      "NEED TO REBUILD COLLECTOR QUEUE"
                      GO TO END-ROUTINE.

           MOVE "R"           TO VCQUPD-ACTION.            *> REWRITE
           MOVE CQ-PATH-OWNBR TO VCQUPD-BRNO.
           MOVE CQ-REC        TO VCQUPD-CQ-REC.
           MOVE SPACES        TO VCQUPD-VCQ-REC.
           PERFORM UPDATE-VCQ1-FILE-VCQUPD.

           IF ( IO-BAD )
              DISPLAY MESSAGE BOX
                      "ERROR: QUEUE RECORD NOT FOUND [2]", X"0A"
                      " ", X"0A"
                      "COLLECTOR ACCOUNT: ",
                       SHOW-BRNO-ACCT-COLLID, X"0A"
                      "STOP!!! CALL BRANCH SUPPORT IMMEDIATELY!!!"
                      "NEED TO REBUILD COLLECTOR QUEUE"
                      GO TO END-ROUTINE.

           GO TO END-WRITE-MODULE.
       
       WRITE-RECORD.

           PERFORM WRITE-CQ1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
              GO TO END-WRITE-MODULE.

           MOVE "W"           TO VCQUPD-ACTION.            *> WRITE
           MOVE CQ-PATH-OWNBR TO VCQUPD-BRNO.
           MOVE CQ-REC        TO VCQUPD-CQ-REC.
           MOVE SPACES        TO VCQUPD-VCQ-REC.
           PERFORM UPDATE-VCQ1-FILE-VCQUPD.
       
       END-WRITE-MODULE.
           PERFORM CLOSE-CQ1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CQ1-FILE.
           PERFORM READ-CQ1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF CQ-REC NOT = ORIGINAL-CQ-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CQ1-FILE.

           MOVE EXT-FILPATH-BASE    TO VCQ-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO VCQ-ALL-MACHINE.
           MOVE EXT-FILPATH-BRNO    TO VCQ-ALL-BRNO.
           MOVE VCQ-ALL-PATH        TO VCQ-PATH.

           MOVE CQ-BRNO   TO VCQ-BRNO.
           MOVE CQ-ACCTNO TO VCQ-ACCTNO.
           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.
           PERFORM DELETE-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-CQ1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CQ-FILE.

           MOVE EXT-CONAME-USER-LOC TO CQ-BRNO.   
           MOVE HOLD-ACCTNO         TO CQ-ACCTNO.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE " "             TO DISPLAY-BUF.
           MOVE CQ-COLLID       TO F01 CDB-CODE.
           MOVE "CN"            TO CDB-TYPE.
           MOVE EXT-FILPATH-BRANCH TO CDB-BRANCH.
           PERFORM VERIFY-CD.
           MOVE CDB-DESC        TO F011.
           COMPUTE F02 = 10 - CQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG       TO F03.
      
           MOVE CQ-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F04.
      
           MOVE CQ-START-TIME    TO F05.
           MOVE CQ-REV-DEL       TO F06.
           MOVE CQ-REV-PDUE-DAYS TO F07.
           MOVE CQ-REV-BAL       TO F08.
           MOVE CQ-WORKED-FG     TO F09.

           IF ( CQ-WORK-COLLID NOT = " " )
              MOVE CQ-WORK-COLLID TO F10 CDB-CODE
              MOVE "CN"           TO CDB-TYPE
              MOVE EXT-FILPATH-BRANCH     TO CDB-BRANCH
              PERFORM VERIFY-CD
              MOVE CDB-DESC     TO F101.
           MOVE CQ-IN-PROCESS TO F11.

           MOVE CQ-DATE-QUEUED      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F12.

           MOVE CQ-TIME-ZONE-ERROR TO F13.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************
           MOVE "CN"               TO CDBSCN-TYPE.
           MOVE EXT-FILPATH-BRANCH TO CDBSCN-BRANCH.
           MOVE SPACES             TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           MOVE "S  A030  0."  TO VDU.
           MOVE ELE            TO VDUROW.
           MOVE CDBSCN-CODE    TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-CODE-SCAN.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO CQMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CQMAIN-SYSDATE.
           DISPLAY SPACES UPON CQMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CQMAIN-SCREEN UPON CQMAIN-WINDOW-HANDLE.
           MOVE CQMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CQMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE CQMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CQMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBSP/SPCQCL.CPY".

      **************************************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************************************
       
       COPY "LIBGB/ACCESS.CPY".
       VERIFY-BRNO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO BR-NAME.
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       VERIFY-CD.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO CDB-DESC.
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
       SEND-LEGEND.
           MOVE "S  A070MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPCQ1IN.CPY".
       COPY "LIBSP/SPVCQ1IN.CPY".
       COPY "LIBSP/SPVCQ2RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/CPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY CQMAIN-WINDOW-HANDLE
           EXIT PROGRAM.
