       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VCQMAN.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   VCQ FILE MAINTENANCE
      *
      *
      *
      *-----------------------------------------------------------------
      * REV:
      * SMR 042225 WROTE PROGRAM (COPIED FROM GRMAIN)
      *
      *
      *                    
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBSP/SPFSVCQ.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBSP/SPFDVCQ.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/VCQMAN S35 04/30/25-10:43:23 >".

      ******************************************************************
      *
      *    F I L E - R E C - W O R K E R S 
      *
      ******************************************************************

      *COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
      *COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBLP/LP01CD.CPY".
      *COPY "LIBLP/LP01CD_SQL.CPY".
      *COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBSP/SP01CQ.CPY".
      *COPY "LIBSP/SP01VCQ.CPY".
       COPY "LIBSP/SPWSVCQ.CPY".

      *=================================================================
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      *=================================================================
       01  ACTION.
           03  CHKEY                    PIC X(04).
           03  CHNO                     PIC 9(02).

       01  CHAN                         PIC X(04)    VALUE "CHAN".

       01  ELE                          PIC 9(02).
           88  ELE-MEMBER-ENTRY                      VALUE 26.

       01  ERRCD                        PIC X(01)    VALUE SPACES.
       01  HOLD-KEY                     PIC X(04).
       01  IO-TYPE                      PIC X(03).
       01  HOLD-ACCTNO.
           03  NUM-ACCTNO               PIC 9(08).
       01  HOLD-BRNO.
           03  NUM-BRNO                 PIC 9(04).

       01  LEGEND                       PIC X(73).
       01  LEGEND-LIST                  PIC X(07)    VALUE "LEGEND.".

       01  MEMBER-SPEC.
           03  MEMBER-ACT               PIC X(01)    VALUE "E".
           03  FILLER                   PIC X(10)    VALUE "  A040990-".
           03  MEMBER-NUM               PIC 9(03)    VALUE ZEROES.
           03  FILLER                   PIC X(01)    VALUE ".".

       01  NEW-FLAG                     PIC 9(01).
       01  ONE                          PIC S9(1).
       01  FIRST-SCAN                   PIC 9(01)     VALUE 0.

       01  RECORD-SIZES.
           03  CHANGED-REC              PIC X(VCQ-SIZE) VALUE SPACES.
           03  ORIGINAL-REC             PIC X(VCQ-SIZE) VALUE SPACES.

      *=================================================================
      *
      *    L E G E N D - E L E M E N T S
      *
      *=================================================================
       01  LEGEND-ELEMENTS.

           03  LEGEND-ELEMENT-010.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-COLLID       ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-020.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-REV-BEHAVIOR-".
               05  FILLER               PIC X(33)    VALUE
                   "RISK                            :".

           03  LEGEND-ELEMENT-030.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-SEQ-FG       ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-040.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-DATE         ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-050.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-START-TIME   ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-060.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-REV-DEL      ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-070.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-REV-PDUE-DAYS".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-080.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-REV-BAL      ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-090.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER VCQ-SEQ          ".
               05  FILLER               PIC X(33)    VALUE
                   "CH NUMBER                       :".

           03  LEGEND-ELEMENT-100.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-BRNO          ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-110.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-ACCTNO        ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".           
           03  LEGEND-ELEMENT-120.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-COLLID        ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-130.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-REV-BEHAVIOR-R".
               05  FILLER               PIC X(33)    VALUE
                   "ISK                             :".
           03  LEGEND-ELEMENT-140.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-SEQ-FG        ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-150.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-DATE          ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-160.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-START-TIME    ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-170.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-REV-DEL       ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-180.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-REV-PDUE-DAYS ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".               
           03  LEGEND-ELEMENT-190.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-REV-BAL       ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-200.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-SEQ           ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-210.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-LTOUCH-DATE   ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-220.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-WORK-COLLID   ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-230.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-WORKED-FG     ".
               05  FILLER               PIC X(33)    VALUE            
                   "                                :".
           03  LEGEND-ELEMENT-240.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-IN-PROCESS    ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".
           03  LEGEND-ELEMENT-250.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-DATE-QUEUED   ".
               05  FILLER               PIC X(33)    VALUE 
                   "                                :".
           03  LEGEND-ELEMENT-260.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CQ-TIME-ZONE-ERRO".
               05  FILLER               PIC X(33)    VALUE
                   "R                               :".

      *-----------------------------------------------------------------
       01  LEGEND-ELEMENTS-TABLE        REDEFINES LEGEND-ELEMENTS.
           03  LEGEND-ELE               PIC X(73)    OCCURS 26 TIMES.

      *-----------------------------------------------------------------
       01  LEGEND-ELEMENT-ACCT.
           05  FILLER               PIC X(42)    VALUE
               "F6-SCAN ACCOUNT #S, F7-EXIT, ENTER ACCOUNT".  
           05  FILLER               PIC X(32)    VALUE
               " #                             :".

       01  LEGEND-ELEMENT-SEL.
           05  FILLER               PIC X(40)    VALUE
               "F1-CLEAR SCREEN, 'DELETE', ENTER FIELD N".
           05  FILLER               PIC X(33)    VALUE
               "UMBER                           :".

      *=================================================================
      *
      *    D I S P L A Y - B U F F E R S
      *
      *-----------------------------------------------------------------
      *    SCREEN DISPLAY BUFFER
      *=================================================================
       01  DISPLAY-BUF                                     VALUE SPACES.
           03  F01                         PIC X(03).
           03  F02                         PIC 9(01).
           03  F03                         PIC X(01).
           03  F04                         PIC 9(08).
           03  F05                         PIC 9(05).
           03  F06                         PIC 9(05).
           03  F07                         PIC 9(05).
           03  F08                         PIC 9(07).
           03  F09                         PIC 9(03).
           03  F10                         PIC 9(04).
           03  F11                         PIC 9(08).
           03  F12                         PIC X(03).    
           03  F13                         PIC 9(01).
           03  F14                         PIC X(01).
           03  F15                         PIC 9(08).
           03  F16                         PIC 9(05).
           03  F17                         PIC 9(05).
           03  F18                         PIC 9(05).
           03  F19                         PIC 9(07).
           03  F20                         PIC 9(03).
           03  F21                         PIC 9(08).
           03  F22                         PIC X(03).
           03  F23                         PIC X(01).
           03  F24                         PIC X(01).
           03  F25                         PIC 9(08).
           03  F26                         PIC X(01).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER                      PIC X(48)       VALUE
               "010 020 030 040 050 060 070 080 090 100 110 120 ".
           03  FILLER                      PIC X(48)       VALUE
               "130 140 150 160 170 180 190 200 210 220 230 240 ".
           03  FILLER                      PIC X(08)       VALUE
               "250 260.".

      *=================================================================
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      *=================================================================
       01  MAX                          PIC 9(02)     VALUE 26.
       01  SPEC-TABLE.
      * 1. VCQ-COLLID
           03  FILLER                   PIC X(08)     VALUE "E  A030 ".
      * 2. VCQ-REV-BEHAVIOR-RISK
           03  FILLER                   PIC X(08)     VALUE "ENNN010 ".
      * 3. VCQ-SEQ-FG   
           03  FILLER                   PIC X(08)     VALUE "E  A010 ".
      * 4. VCQ-DATE          
           03  FILLER                   PIC X(08)     VALUE "ENNN080 ".
      * 5. VCQ-START-TIME 
           03  FILLER                   PIC X(08)     VALUE "ENNS040 ".
      * 6. VCQ-REV-DEL 
           03  FILLER                   PIC X(08)     VALUE "ENNN050 ".
      * 7. VCQ-REV-PDUE-DAYS
           03  FILLER                   PIC X(08)     VALUE "ENNN050 ".
      * 8. VCQ-REV-BAL
           03  FILLER                   PIC X(08)     VALUE "ENNN070 ".
      * 9. VCQ-SEQ       
           03  FILLER                   PIC X(08)     VALUE "ENNN030 ".
      *10. CQ-BRNO          
           03  FILLER                   PIC X(08)     VALUE "ENNN040 ".
      *11. CQ-ACCTNO            
           03  FILLER                   PIC X(08)     VALUE "ENNN080 ".
      *12. CQ-COLLID
           03  FILLER                   PIC X(08)     VALUE "E  A030 ".
      *13. CQ-REV-BEHAVIOR-RISK
           03  FILLER                   PIC X(08)     VALUE "ENNN010 ".
      *14. CQ-SEQ-FG
           03  FILLER                   PIC X(08)     VALUE "E  A010 ".
      *15. CQ-DATE
           03  FILLER                   PIC X(08)     VALUE "ENNN080 ".
      *16. CQ-START-TIME
           03  FILLER                   PIC X(08)     VALUE "ENNS040 ".
      *17. CQ-REV-DEL
           03  FILLER                   PIC X(08)     VALUE "ENNN050 ".
      *18. CQ-REV-PDUE-DAYS
           03  FILLER                   PIC X(08)     VALUE "ENNN050 ".
      *19. CQ-REV-BAL
           03  FILLER                   PIC X(08)     VALUE "ENNN070 ".
      *20. CQ-SEQ
           03  FILLER                   PIC X(08)     VALUE "ENNN030 ".
      *21. CQ-LTOUCH-DATE
           03  FILLER                   PIC X(08)     VALUE "ENNN080 ".
      *22. CQ-WORK-COLLID
           03  FILLER                   PIC X(08)     VALUE "E  A030 ".
      *23. CQ-WORKED-FG
           03  FILLER                   PIC X(08)     VALUE "E  A010 ".
      *24. CQ-IN-PROCESS
           03  FILLER                   PIC X(08)     VALUE "E  A010 ".
      *25. CQ-DATE-QUEUED
           03  FILLER                   PIC X(08)     VALUE "ENNN080 ".
      *26. CQ-TIME-ZONE-ERROR 
           03  FILLER                   PIC X(08)     VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TABLE2        REDEFINES SPEC-TABLE.
           03  SPEC-REC                 OCCURS 26 TIMES.
               05  SPEC                 PIC X(07).
               05  CHFG                 PIC X(01).

       01  VCQMAN-WINDOW-HANDLE  PIC X(10).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/VCQUPD_LNK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PHONE.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/VCQMAN_DEF.CPY".
       COPY "LIBMN/VCQMAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBMN/VCQMAN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               VCQ-FILE.
       
       COPY "LIBGB/DECLARE.CPY".
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "VCQMAN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS VCQMAN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           MOVE EXT-FILPATH-BASE          TO VCQ-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE       TO VCQ-ALL-MACHINE.
           MOVE EXT-FILPATH-BRNO          TO VCQ-ALL-BRNO.
           MOVE VCQ-ALL-PATH              TO VCQ-PATH.

       MAIN-MODULE.
      D    STOP MESS.

      * CHECK IF VCQFILE EXISTS OR NOT
           MOVE VCQ-ALL-PATH              TO ACCESS-BUF.
           
           PERFORM ACCESS-CALL.

           IF STAT-GOOD
              PERFORM OPEN-VCQ1-FILE
           ELSE
              PERFORM OPEN-VCQ1-FILE-OUTPUT
              PERFORM CLOSE-VCQ1-FILE
              PERFORM OPEN-VCQ1-FILE.

           PERFORM START-VCQ1-FILE.

           PERFORM FORM-RESET.

           PERFORM REC-BRANCH-MODULE.
           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
      D    STOP MESS.
           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( IO-TYPE = "DEL" )
              PERFORM DELETE-MODULE.
 
           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BRANCH-MODULE SECTION.
      ******************************************************************
       REC-BRANCH-MODULE-NO.

           MOVE ZEROES TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

      * MOVE THE USER'S BRANCH # TO THE BRANCH FIELDS
           MOVE VCQ-ALL-BRNO        TO HOLD-BRNO. 
  
           MOVE LEGEND-ELEMENT-ACCT TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-KEY.

           MOVE "S  A040BRAN." TO VDU.
           MOVE HOLD-BRNO      TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A080ACCT." TO VDU.
           PERFORM SCREENIO. 

           MOVE "E  A080ACCT." TO VDU.
           PERFORM SCREENIO.

      * F6
           IF ( VDUSTATUS = "1<" )
              PERFORM ENTER-SCANKEY       
              GO TO END-REC-BRANCH-MODULE.                              

      * F7
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BRANCH-MODULE.

      * NO ENTRY, PROMPT AGAIN FOR ACCT # 
           IF VDUNUM0 = SPACES
              GO TO ENTER-KEY.
      D    STOP MESS.

      * ACCT # VALUE ENTERED
           MOVE HOLD-BRNO           TO VCQ-BRNO.
           MOVE VDUBUF              TO HOLD-ACCTNO.
           MOVE NUM-ACCTNO          TO VCQ-ACCTNO.

      *    IF ( VDUSTATUS NOT = "00" )
      *       GO TO REC-BRANCH-MODULE-NO.

      *    IF ( VDUBUF = SPACES )
      *       GO TO REC-BRANCH-MODULE-NO.
      *-----------------------------------------------------------------
      *READ-GROUP-FILE.

      *    PERFORM READ-VCQ1-FILE-NEXT.
           PERFORM READ-VCQ1-FILE.
           PERFORM CLOSE-VCQ1-FILE.

           IF ( IO-FG = ZEROES )
              MOVE VCQ-REC TO ORIGINAL-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-BRANCH-MODULE.
           EXIT.

      *****************************************************************
       ENTER-SCANKEY SECTION.       
      *****************************************************************
           PERFORM OPEN-VCQ1-FILE.

           MOVE NUM-BRNO          TO VCQ-BRNO.
           MOVE 0                 TO VCQ-ACCTNO.

           PERFORM START-VCQ1-FILE.
          
           MOVE "S  A080ACCT."    TO VDU.
           MOVE "0"               TO VDUBUF.
           PERFORM SCREENIO. 

       ENTER-SCAN.
      *    MOVE "EN N120SCANKEY." TO VDU.
           MOVE "E  A120SCANKEY." TO VDU.
           PERFORM SCREENIO.

      * <F1>
           IF VDUSTATUS = "10"
              MOVE "CLR"          TO IO-TYPE
              GO TO END-SCANKEY.
     
      * <F7>
           IF VDUSTATUS = "1>"
              MOVE "END"          TO IO-TYPE
              GO TO END-SCANKEY.
 
      * <F6>
           IF VDUSTATUS = "1<" 
              GO TO READ-FILES-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCAN.

           MOVE NUM-BRNO          TO VCQ-BRNO.
           MOVE NUM-ACCTNO        TO VCQ-ACCTNO.

           PERFORM START-VCQ1-FILE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

       READ-FILES-NEXT.
  
           PERFORM READ-VCQ1-FILE-NEXT.

           IF IO-BAD
              MOVE "CLR"          TO IO-TYPE
              PERFORM DISPLAY-ENDFILE
              GO TO CLOSE-SCANKEY.
  
           PERFORM DISPLAY-SCANKEY.
           PERFORM DISPLAY-FIELDS.

           MOVE 1                 TO FIRST-SCAN.
           GO TO ENTER-SCAN.

       DISPLAY-SCANKEY.

           MOVE "S  A080ACCT."    TO VDU.
      *    MOVE VCQ-ACCTNO        TO VDUBUF. 
           MOVE VCQ-ACCTNO        TO NUM-ACCTNO.
           MOVE HOLD-ACCTNO       TO VDUBUF.
           PERFORM SCREENIO. 

       DISPLAY-ENDFILE.

           MOVE "S  A000NEW."        TO VDU.
           MOVE "* END OF RECORDS *" TO VDUBUF.
           PERFORM SCREENIO.

       CLOSE-SCANKEY.

           MOVE 0                 TO FIRST-SCAN.
             
           PERFORM CLOSE-VCQ1-FILE.

       END-SCANKEY.
           EXIT.               

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE LEGEND-ELEMENT-SEL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO = ZEROES )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG (ELE) = SPACES )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
           ELSE
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           MOVE LEGEND-ELE(ELE) TO LEGEND.
           PERFORM SEND-LEGEND.

      * GO TO 'MEMBER' ENTRY SECTION; NEED TO CHANGE
      * 'ELE-MEMBER-ENTRY' IF ADDING A NEW ELEMENT.

      *    IF ( ELE-MEMBER-ENTRY )
      *       GO TO F-99.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      * 'F1' KEY ROUTINE: CLEAR SCREEN

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE
                         GO TO END-ENTRY-MODULE.

      * 'F6' KEY ROUTINE: CODE SCAN FOR GROUP LEVEL

           IF ( F6-KEY )
              IF ( ELE = 2 )
                 PERFORM CODE-SCAN
                 IF ( IO-FG = ZEROES )
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.

      * 'UP' KEY ROUTINE:

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO GOTO-FIELD.

      * 'DOWN' KEY ROUTINE:

           IF ( DOWN-KEY ) MOVE 1 TO ONE
                           GO TO GOTO-FIELD.


           IF ( NOT ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUBUF TO VCQ-COLLID.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-02.
           MOVE VDUNUM0 TO VCQ-REV-BEHAVIOR-RISK.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO VCQ-SEQ-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM0 TO VCQ-DATE. 
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO VCQ-START-TIME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM0 TO VCQ-REV-DEL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM0 TO VCQ-REV-PDUE-DAYS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM0 TO VCQ-REV-BAL. 
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM0 TO VCQ-SEQ.         
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM0 TO CQ-BRNO.               
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUNUM0 TO CQ-ACCTNO. 
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-12. 
           MOVE VDUBUF  TO CQ-COLLID.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM0 TO CQ-REV-BEHAVIOR-RISK.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-14.
           MOVE VDUBUF  TO CQ-SEQ-FG.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-15.
           MOVE VDUNUM0 TO CQ-DATE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-16.
           MOVE VDUNUM0 TO CQ-START-TIME.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-17.
           MOVE VDUNUM0 TO CQ-REV-DEL.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-18.
           MOVE VDUNUM0 TO CQ-REV-PDUE-DAYS.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-19.
           MOVE VDUNUM0 TO CQ-REV-BAL.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-20.
           MOVE VDUNUM0 TO CQ-SEQ.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-21.
           MOVE VDUNUM0 TO CQ-LTOUCH-DATE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-22.
           MOVE VDUBUF  TO CQ-WORK-COLLID.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-23.
           MOVE VDUBUF  TO CQ-WORKED-FG.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-24.
           MOVE VDUBUF  TO CQ-IN-PROCESS.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-25.
           MOVE VDUNUM0 TO CQ-DATE-QUEUED.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-26.
           MOVE VDUBUF     TO CQ-TIME-ZONE-ERROR.
           MOVE NUM-ACCTNO TO VCQ-ACCTNO.
           MOVE CQ-REC     TO VCQ-CQ-REC.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

      *    MOVE "LV"       TO CDSCAN-TYPE.
      *    MOVE ZEROES     TO CDSCAN-LV-LEVEL.
      *    MOVE SPACES     TO CDSCAN-LV-FILLER.

      *    MOVE "WI/CDSCAN" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
      *    CANCEL FORM-PROGX.
      *    IF ( CDSCAN-STATUS NOT  = "00") OR (CDSCAN-LV-LEVEL = ZEROES)
      *       MOVE 9 TO IO-FG
      *       GO TO EXIT-CODE-SCAN.

      *    MOVE "SNNN010020." TO VDU.
      *    MOVE CDSCAN-LV-LEVEL  TO VDUNUM0.
      *    PERFORM SCREENIO.

      *    MOVE CDSCAN-LV-LEVEL TO VDUNUM0.
      *    MOVE ZEROES       TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
      D    STOP MESS.
           PERFORM OPEN-VCQ1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CQ-REC  TO VCQ-CQ-REC.
           MOVE VCQ-REC TO CHANGED-REC.

           IF ORIGINAL-REC = CHANGED-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-VCQ1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF VCQ-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-REC TO VCQ-REC.
           PERFORM REWRITE-VCQ1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-VCQ1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-VCQ1-FILE.

       EXIT-WRITE-MODULE.
           EXIT.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-VCQ1-FILE.
           PERFORM READ-VCQ1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF VCQ-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-VCQ1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-VCQ1-FILE.

       EXIT-DELETE-MODULE.
           EXIT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1                    TO NEW-FLAG.

           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES   TO VCQ-REC.
           MOVE NUM-BRNO TO VCQ-BRNO.

           MOVE ZEROES   TO ELE.

       EXIT-NEW-RECORD.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
      D    STOP MESS.
           MOVE VCQ-COLLID              TO F01.
           MOVE VCQ-REV-BEHAVIOR-RISK   TO F02.
           MOVE VCQ-SEQ-FG              TO F03.
           MOVE VCQ-DATE                TO F04.
           MOVE VCQ-START-TIME          TO F05.
           MOVE VCQ-REV-DEL             TO F06.    
           MOVE VCQ-REV-PDUE-DAYS       TO F07.
           MOVE VCQ-REV-BAL             TO F08. 
           MOVE VCQ-SEQ                 TO F09.

           MOVE VCQ-CQ-REC              TO CQ-REC.

           MOVE CQ-BRNO                 TO F10.
           MOVE CQ-ACCTNO               TO F11.
           MOVE CQ-COLLID               TO F12.
           MOVE CQ-REV-BEHAVIOR-RISK    TO F13.
           MOVE CQ-SEQ-FG               TO F14. 
           MOVE CQ-DATE                 TO F15.
           MOVE CQ-START-TIME           TO F16.    
           MOVE CQ-REV-DEL              TO F17.
           MOVE CQ-REV-PDUE-DAYS        TO F18.
           MOVE CQ-REV-BAL              TO F19.
           MOVE CQ-SEQ                  TO F20.
           MOVE CQ-LTOUCH-DATE          TO F21.
           MOVE CQ-WORK-COLLID          TO F22.
           MOVE CQ-WORKED-FG            TO F23.           
           MOVE CQ-IN-PROCESS           TO F24.     
           MOVE CQ-DATE-QUEUED          TO F25.
           MOVE CQ-TIME-ZONE-ERROR      TO F26.

       END-DISPLAY-FIELDS.

           MOVE DISPLAY-BUF             TO WR-BUF.
           MOVE DISPLAY-LIST            TO WR-LIST.
           PERFORM CALL-WRFORM. 

       EXIT-DISPLAY-FIELDS.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
      *COPY "LIBGB/GBGRCL.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO VCQMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO VCQMAN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY VCQMAN-SCREEN UPON VCQMAN-WINDOW-HANDLE.
           MOVE VCQMAN-FORM-FIELDS TO WR-BUF.
           MOVE VCQMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE VCQMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE VCQMAN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/VCQMAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
      *COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *COPY "LIBSP/SPFRVCQ.CPY".

       GET-CD-LV.
      *    MOVE "LV"     TO CD-TYPE.
      *    MOVE GR-LEVEL TO CD-LV-LEVEL.
      *    MOVE SPACES   TO CD-LV-FILLER.
      *    PERFORM OPEN-CD1-FILE.
      *    PERFORM READ-CD1-FILE.
      *    PERFORM CLOSE-CD1-FILE.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-DISPLAY.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
      *COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBSP/SPVCQ1IN.CPY".    
      *COPY "LIBLP/LPCDGS_SQL.CPY".
      *COPY "LIBGB/GBGRGS_SQL.CPY".
      *COPY "LIBGB/GBBRGS_SQL.CPY".
      *COPY "LIBGB/GBBRRN.CPY".
      *COPY "LIBGB/GBGR1IN.CPY".
      *COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           PERFORM FORM-RESET.
           DESTROY VCQMAN-SCREEN.
           DESTROY VCQMAN-WINDOW-HANDLE.
           EXIT PROGRAM.
