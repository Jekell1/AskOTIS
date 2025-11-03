      * COPYMEMBER: LIBFP/PST.INFO
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST.INFO
      *
      * DESCRIPTION   :  LASER (PST) FORM INFO MODULE
      *
      * NEEDED ITEMS  :  BR-FILE
      *                  BW-FILE
      *                  CD-FILE
      *                  LTC-FILE
      *
      * INPUT  (IN)   :  LN-REC
      *                  SP-REC
      *
      * OUTPUT (OUT)  :  INFORMATION FOR BORROWERS/COLLATERAL/FEES
      *
      *=================================================================
      * REV:
      * KEC 2007-1101 ***NEW***
      * KEC 2008-1015 NEEDED TO ADD THE OPEN OF THE LP-FILE.
      * KEC 2009-0611 ADDED BORROWERS PRINT LAYOUT (PBL).
      * KEC 2009-0826 {PR#03423} REGENCY
      *               ADDED LOGIC TO USE THE PD FILE (SAVED LOAN) AND
      *               THE PC FILE (SAVED COLLATERAL).
      * KEC 2011-0930 ***NEW*** POSTSCRIPT
      * BAH 2016-0114 SPLIT LT-FILE, LTC-FILE 
      * KEC 2016-0314 CHANGED CD-FEE???? TO CD-GC-????.
      * BAH 2018-1008 GLOBAL CKFILE
      * BAH 2018-1009 GLOBAL CXFILE
      * BAH 2018-1022 ADDED DEBIT CARD NETSPEND PROCEEDS AS
      *               CHECK-AMT-TO-BORROWER, WORLD #1344
      * BAH 2020-0617 CHANGED PC-LT-RECORD TO PC-LTC-RECORD
      * JKC 2021-1014 COMMENTED OUT 'PST-INFO-BPL-ROUTINE SECTION.' 
      *          SINCE IT WAS DEAD CODE (NOT USED ANYWHERE).
      * BAH 2022-0503 REMOVED SEARCHING THE CKFILE FOR CHECKS, NEEDED TO USE
      *               TRCFILE. ADDED SEARCHING THE TRCFILE.
      *
      ******************************************************************
      *
      *    P S T - I N F O - M O D U L E
      *
      ******************************************************************
       PST-INFO-MODULE SECTION.

           PERFORM PST-INFO-INITIALIZATION.
           PERFORM PST-INFO-SETUP-BRANCH.
           PERFORM PST-INFO-SETUP-BORROWERS.
           PERFORM PST-INFO-SETUP-CHECKS.
           PERFORM PST-INFO-SETUP-CODES.
           PERFORM PST-INFO-SETUP-COLLATERAL.
           PERFORM PST-INFO-SETUP-FEES.
           PERFORM PST-INFO-SETUP-INSURANCE.
           PERFORM PST-INFO-SETUP-LOAN.
           PERFORM PST-INFO-SETUP-MON-DIS-BEN.

       PST-INFO-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - I N I T I A L I Z A T I O N
      *
      *-----------------------------------------------------------------
      *    DESC:  INITIALIZE FIELDS FOR USING THESE FIELDS
      *    FROM:  PST-INFO-ROUTINE
      ******************************************************************
       PST-INFO-INITIALIZATION SECTION.

           IF ( PST-INFO-FIRST-TIME = "Y" )
              PERFORM OPEN-GB-FILE
              MOVE GB-PATH-OWNBR TO GB-BRNO
              MOVE 1             TO GB-NO
              PERFORM READ-GB-FILE
              PERFORM CLOSE-GB-FILE
              IF NOT ( IO-GOOD )
                 MOVE "GLOBAL RECORD MISSING" TO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           PERFORM LOAN-CALCULATIONS.
           PERFORM MATURITY-DATE-CALCULATION.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-CK1-FILE.
           PERFORM OPEN-TRC1-FILE.
           PERFORM OPEN-CX1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LTC1-FILE.
           PERFORM OPEN-PC1-FILE.
           PERFORM OPEN-SP1-FILE.

       PST-INFO-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - B R A N C H
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE BRANCH INFORMATION
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  BR-REC
      *    OUT :  BRANCH-ADDRESS
      *           BRANCH-CSZ
      *           BRANCH-NAME
      ******************************************************************
       PST-INFO-SETUP-BRANCH SECTION.

           MOVE SPACES TO BRANCH-INFORMATION-WORKERS.

           STRING BR-ADR1, " ", BR-ADR2
                  DELIMITED BY "  " INTO BRANCH-ADDRESS.
           STRING BR-CITY, ", ", BR-STATE, " ", BR-ZIP
                  DELIMITED BY "  " INTO BRANCH-CSZ.

           MOVE BR-NAME TO BRANCH-NAME.

       PST-INFO-SETUP-BRANCH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - B O R R O W E R
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE BORROWER INFORMATION
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  BW-REC
      *           LN-SSNO(1~4)
      *    OUT :  ENDR1-INFO, ENDR2-INFO, ENDR3-INFO
      *           MAKER-INFO, JOINT-INFO, SPOUS-INFO
      ******************************************************************
       PST-INFO-SETUP-BORROWERS SECTION.

           PERFORM PST-INFO-PIBW-CLEAR-FIELDS.

      * FIND 'JOINT' 
           IF ( PST-INFO-FORCE-JOINT-BW-2ND ) AND
              ( LN-SSNO(2) NOT = ZEROES     )
              MOVE 2 TO PIBW-SUB-JOINT
           ELSE
           IF ( PST-INFO-FORCE-JOINT-BW-3RD ) AND
              ( LN-SSNO(3) NOT = ZEROES     )
              MOVE 3 TO PIBW-SUB-JOINT
           ELSE
           IF ( PST-INFO-FORCE-JOINT-BW-4TH ) AND
              ( LN-SSNO(4) NOT = ZEROES     )
              MOVE 4 TO PIBW-SUB-JOINT
           ELSE
           IF ( LN-SSNO(2) NOT = ZEROES ) AND ( LN-MAKERCD(2) = "J" )
              MOVE 2 TO PIBW-SUB-JOINT
           ELSE
           IF ( LN-SSNO(3) NOT = ZEROES ) AND ( LN-MAKERCD(3) = "J" )
              MOVE 3 TO PIBW-SUB-JOINT
           ELSE
           IF ( LN-SSNO(4) NOT = ZEROES ) AND ( LN-MAKERCD(4) = "J" )
              MOVE 4 TO PIBW-SUB-JOINT.

      * FIND 'ENDR1' 
           IF ( LN-SSNO(2) NOT = ZEROES ) AND ( PIBW-SUB-JOINT NOT = 2 )
              MOVE 2 TO PIBW-SUB-ENDR1
           ELSE
           IF ( LN-SSNO(3) NOT = ZEROES ) AND ( PIBW-SUB-JOINT NOT = 3 )
              MOVE 3 TO PIBW-SUB-ENDR1
           ELSE
           IF ( LN-SSNO(4) NOT = ZEROES ) AND ( PIBW-SUB-JOINT NOT = 4 )
              MOVE 4 TO PIBW-SUB-ENDR1.

      * FIND 'ENDR2' 
           IF ( LN-SSNO(2)    NOT = ZEROES ) AND
              ( PIBW-SUB-JOINT NOT = 2      ) AND
              ( PIBW-SUB-ENDR1 NOT = 2      )
              MOVE 2 TO PIBW-SUB-ENDR2
           ELSE
           IF ( LN-SSNO(3)    NOT = ZEROES ) AND
              ( PIBW-SUB-JOINT NOT = 3      ) AND
              ( PIBW-SUB-ENDR1 NOT = 3      )
              MOVE 3 TO PIBW-SUB-ENDR2
           ELSE
           IF ( LN-SSNO(4)    NOT = ZEROES ) AND
              ( PIBW-SUB-JOINT NOT = 4      ) AND
              ( PIBW-SUB-ENDR1 NOT = 4      )
              MOVE 4 TO PIBW-SUB-ENDR2.

      * FIND 'ENDR3' 
           IF ( LN-SSNO(2)    NOT = ZEROES ) AND
              ( PIBW-SUB-JOINT NOT = 2      ) AND
              ( PIBW-SUB-ENDR1 NOT = 2      ) AND
              ( PIBW-SUB-ENDR2 NOT = 2      )
              MOVE 2 TO PIBW-SUB-ENDR3
           ELSE
           IF ( LN-SSNO(3)    NOT = ZEROES ) AND
              ( PIBW-SUB-JOINT NOT = 3      ) AND
              ( PIBW-SUB-ENDR1 NOT = 3      ) AND
              ( PIBW-SUB-ENDR2 NOT = 3      )
              MOVE 3 TO PIBW-SUB-ENDR3
           ELSE
           IF ( LN-SSNO(4)    NOT = ZEROES ) AND
              ( PIBW-SUB-JOINT NOT = 4      ) AND
              ( PIBW-SUB-ENDR1 NOT = 4      ) AND
              ( PIBW-SUB-ENDR2 NOT = 4      )
              MOVE 4 TO PIBW-SUB-ENDR3.

      * SETUP 'MAKER' 
           MOVE PIBW-MAKER TO PIBW-ELE.
           MOVE 1         TO PIBW-SUB.
           PERFORM PST-INFO-PIBW-LOAD-FIELDS.

      * SETUP 'JOINT'
           IF ( PIBW-SUB-JOINT NOT = ZEROES )
              MOVE PIBW-JOINT     TO PIBW-ELE
              MOVE PIBW-SUB-JOINT TO PIBW-SUB
              PERFORM PST-INFO-PIBW-LOAD-FIELDS.

      * SETUP 'ENDR1'
           IF ( PIBW-SUB-ENDR1 NOT = ZEROES )
              MOVE PIBW-ENDR1     TO PIBW-ELE
              MOVE PIBW-SUB-ENDR1 TO PIBW-SUB
              PERFORM PST-INFO-PIBW-LOAD-FIELDS.

      * SETUP 'ENDR2' 
           IF ( PIBW-SUB-ENDR2 NOT = ZEROES )
              MOVE PIBW-ENDR2     TO PIBW-ELE
              MOVE PIBW-SUB-ENDR2 TO PIBW-SUB
              PERFORM PST-INFO-PIBW-LOAD-FIELDS.

      * SETUP 'ENDR3' 
           IF ( PIBW-SUB-ENDR3 NOT = ZEROES )
              MOVE PIBW-ENDR3     TO PIBW-ELE
              MOVE PIBW-SUB-ENDR3 TO PIBW-SUB
              PERFORM PST-INFO-PIBW-LOAD-FIELDS.

      * SETUP 'SPOUS' 
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF ( IO-BAD )
              GO TO PST-INFO-SETUP-BW-EXIT.

           IF ( BW-SPSSNO = ZEROES )
              GO TO PST-INFO-SETUP-BW-EXIT.

           MOVE BW-SPSSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF ( IO-BAD )
              GO TO PST-INFO-SETUP-BW-EXIT.

           MOVE PIBW-SPOUS TO PIBW-ELE.
           MOVE ZEROES    TO PIBW-SUB.
           PERFORM PST-INFO-PIBW-LOAD-FIELDS.

       PST-INFO-SETUP-BW-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - B P L - R O U T I N E
      *
      *=================================================================
      *    DESC: BORROWERS PRINT LAYOUT (PBL)
      *          USED TO PRINT BORROWER INFO IN A SPECFIC WAY.
      *
      *    IN  : BPL-MAX = 1         MAKER NAME
      *                              JOINT NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      *    IN  : BPL-MAX = 2         MAKER NAME
      *                              JOINT/ENDR1 NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      *    IN  : BPL-MAX = 3         MAKER NAME
      *                              JOINT/ENDR1 NAME
      *                              ENDR1/ENDR2 NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      *    IN  : BPL-MAX = 4         MAKER NAME
      *                              JOINT/ENDR1 NAME
      *                              ENDR1/ENDR2 NAME
      *                              ENDR2/ENDR3 NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      ******************************************************************
      *PST-INFO-BPL-ROUTINE SECTION.

      *    MOVE SPACES                     TO BPL-TABLE.
      *    MOVE ZEROES                     TO BPL-ELE.

      *    IF ( BPL-MAX = ZEROES    ) OR
      *       ( BPL-MAX > BPL---MAX )
      *       GO TO PST-INFO-BPL-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ MAKER NAME (01)  }
      *    ADD  01                         TO BPL-ELE.
      *    MOVE MAKER-NAME-FIRST-LAST      TO BPL-INFO(BPL-ELE).

      *-----------------------------------------------------------------
      *PST-INFO-BPL-ROUTINE-MAX-01.

      *    IF ( BPL-MAX NOT = 01 )
      *       GO TO PST-INFO-BPL-ROUTINE-MAX-02.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ JOINT NAME }
      *    IF ( JOINT-NAME-FIRST-LAST NOT = SPACES )
      *       ADD  01                      TO BPL-ELE
      *       MOVE JOINT-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *-----------------------------------------------------------------
      *PST-INFO-BPL-ROUTINE-MAX-02.

      *    IF ( BPL-MAX NOT = 02 )
      *       GO TO PST-INFO-BPL-ROUTINE-MAX-03.

      *- - - - - - - - - - - - - - - - - - - - - -{ SET STOP SUBSCRIPT }
      *    MOVE 02                         TO BPL-STOP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ JOINT NAME }
      *    IF ( JOINT-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE JOINT-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ ENDR1 NAME }
      *    IF ( ENDR1-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE ENDR1-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *-----------------------------------------------------------------
      *PST-INFO-BPL-ROUTINE-MAX-03.

      *    IF ( BPL-MAX NOT = 03 )
      *       GO TO PST-INFO-BPL-ROUTINE-MAX-04.

      *- - - - - - - - - - - - - - - - - - - - - -{ SET STOP SUBSCRIPT }
      *    MOVE 03                         TO BPL-STOP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ JOINT NAME }
      *    IF ( JOINT-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE JOINT-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ ENDR1 NAME }
      *    IF ( ENDR1-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE ENDR1-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ ENDR2 NAME }
      *    IF ( ENDR2-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE ENDR2-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *-----------------------------------------------------------------
      *PST-INFO-BPL-ROUTINE-MAX-04.

      *    IF ( BPL-MAX NOT = 04 )
      *       GO TO PST-INFO-BPL-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - -{ SET STOP SUBSCRIPT }
      *    MOVE 04                         TO BPL-STOP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ JOINT NAME }
      *    IF ( JOINT-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE JOINT-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ ENDR1 NAME }
      *    IF ( ENDR1-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE ENDR1-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ ENDR2 NAME }
      *    IF ( ENDR2-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE ENDR2-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ ENDR3 NAME }
      *    IF ( ENDR3-NAME-FIRST-LAST NOT = SPACES   ) AND
      *       ( BPL-ELE               NOT = BPL-STOP )
      *       ADD  01                      TO BPL-ELE
      *       MOVE ENDR3-NAME-FIRST-LAST   TO BPL-INFO(BPL-ELE).

      *=================================================================
      *PST-INFO-BPL-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ MAKER ADDR }
      *    ADD  01                         TO BPL-ELE.
      *    MOVE MAKER-ADDRESS              TO BPL-INFO(BPL-ELE).

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ MAKER CSZ  }
      *    ADD  01                         TO BPL-ELE.
      *    MOVE MAKER-CSZ                  TO BPL-INFO(BPL-ELE).

      *-----------------------------------------------------------------
      *PST-INFO-BPL-ROUTINE-EXIT.
      *    EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - F B W - C L E A R - F I E L D S
      *
      *-----------------------------------------------------------------
      *    DESC:  CLEAR ALL PIBW FIELDS
      *    FROM:  PST-INFO-SETUP-BORROWERS
      ******************************************************************
       PST-INFO-PIBW-CLEAR-FIELDS SECTION.

           MOVE ZEROES TO PIBW-ELE
                          PIBW-SUB
                          PIBW-SUB-JOINT
                          PIBW-SUB-ENDR1
                          PIBW-SUB-ENDR2
                          PIBW-SUB-ENDR3.

           PERFORM VARYING PIBW-SUB FROM 1 BY 1
                   UNTIL   PIBW-SUB > PIBW-MAX
               MOVE SPACES TO PIBW-ADDRESS-1           (PIBW-SUB)
               MOVE SPACES TO PIBW-ADDRESS-2           (PIBW-SUB)
               MOVE ZEROES TO PIBW-AGE                 (PIBW-SUB)
               MOVE ZEROES TO PIBW-BDATE               (PIBW-SUB)
               MOVE SPACES TO PIBW-CITY                (PIBW-SUB)
               MOVE SPACES TO PIBW-CODE                (PIBW-SUB)
               MOVE SPACES TO PIBW-COUNTY              (PIBW-SUB)
               MOVE ZEROES TO PIBW-DEPENDENTS          (PIBW-SUB)
               MOVE SPACES TO PIBW-EMPLOYER            (PIBW-SUB)
               MOVE SPACES TO PIBW-POSITION            (PIBW-SUB)
               MOVE SPACES TO PIBW-WORK-ADDR1          (PIBW-SUB)
               MOVE SPACES TO PIBW-WORK-ADDR2          (PIBW-SUB)
               MOVE ZEROES TO PIBW-WORKPHONE           (PIBW-SUB)
               MOVE ZEROES TO PIBW-WORKEXT             (PIBW-SUB)
               MOVE ZEROES TO PIBW-WORK-YEARS          (PIBW-SUB)
               MOVE SPACES TO PIBW-FNAME               (PIBW-SUB)
               MOVE ZEROES TO PIBW-HOMEPHONE           (PIBW-SUB)
               MOVE ZEROES TO PIBW-INCOME              (PIBW-SUB)
               MOVE SPACES TO PIBW-LNAME               (PIBW-SUB)
               MOVE ZEROES TO PIBW-OTHER-INCOME        (PIBW-SUB)
               MOVE SPACES TO PIBW-OTHER-INCOME-SOURCE (PIBW-SUB)
               MOVE ZEROES TO PIBW-SSNO                (PIBW-SUB)
               MOVE SPACES TO PIBW-STATE               (PIBW-SUB)
               MOVE SPACES TO PIBW-NAME-FIRST-LAST     (PIBW-SUB)
               MOVE SPACES TO PIBW-ADDRESS             (PIBW-SUB)
               MOVE SPACES TO PIBW-CSZ                 (PIBW-SUB)
               MOVE SPACES TO PIBW-ADDRESS-CSZ         (PIBW-SUB)
               MOVE SPACES TO PIBW-NAME-ADDRESS-CSZ    (PIBW-SUB)
               MOVE SPACES TO PIBW-NAME-LAST-FIRST     (PIBW-SUB)
               MOVE SPACES TO PIBW-ZIP                 (PIBW-SUB)
           END-PERFORM.

       PST-INFO-PIBW-CLEAR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - F B W - L O A D - F I E L D S
      *
      *-----------------------------------------------------------------
      *    DESC:  LOAD PIBW FIELDS FOR A GIVEN BORROWER
      *    FROM:  PST-INFO-SETUP-BORROWERS
      ******************************************************************
       PST-INFO-PIBW-LOAD-FIELDS SECTION.

           IF ( PIBW-ELE NOT = PIBW-SPOUS )
              MOVE LN-SSNO(PIBW-SUB) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

           MOVE BW-ADR1                  TO PIBW-ADDRESS-1   (PIBW-ELE).
           MOVE BW-ADR2                  TO PIBW-ADDRESS-2   (PIBW-ELE).

      * LOAD 'AGE' & 'BDATE' 
           MOVE BW-BDATE                 TO BAGE-DATE.
           MOVE BW-REFDATE               TO BAGE-REFDATE.
           MOVE TRANS-DATE               TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

           IF ( BW-BDATE > 200 )
              MOVE BAGE-AGE              TO PIBW-AGE         (PIBW-ELE)
              MOVE BW-BDATE              TO PIBW-BDATE       (PIBW-ELE)
           ELSE
              MOVE BW-BDATE              TO PIBW-AGE         (PIBW-ELE)
              MOVE ZEROES                TO PIBW-BDATE       (PIBW-ELE).

           MOVE BW-CITY                  TO PIBW-CITY        (PIBW-ELE).

           IF ( PIBW-SUB = ZEROES )
              MOVE "S"                   TO PIBW-CODE        (PIBW-ELE)
           ELSE
              MOVE LN-MAKERCD(PIBW-SUB)  TO PIBW-CODE        (PIBW-ELE).

           MOVE SPACES                   TO PIBW-COUNTY      (PIBW-ELE).
           MOVE BW-DEPEND                TO PIBW-DEPENDENTS  (PIBW-ELE).
           MOVE BW-EMPLOYER              TO PIBW-EMPLOYER    (PIBW-ELE).
           MOVE BW-EMPLPOS               TO PIBW-POSITION    (PIBW-ELE).
           MOVE BW-EMPPHONE              TO PIBW-WORKPHONE   (PIBW-ELE).
           MOVE BW-EMPEXT                TO PIBW-WORKEXT     (PIBW-ELE).
           MOVE BW-EMPYEARS              TO PIBW-WORK-YEARS  (PIBW-ELE).
           MOVE BW-EMPADDR1              TO PIBW-WORK-ADDR1  (PIBW-ELE).
           MOVE BW-EMPADDR2              TO PIBW-WORK-ADDR2  (PIBW-ELE).
           MOVE BW-FNAME                 TO PIBW-FNAME       (PIBW-ELE).
           MOVE BW-PHONE                 TO PIBW-HOMEPHONE   (PIBW-ELE).
           MOVE BW-EMPPAY                TO PIBW-INCOME      (PIBW-ELE).
           MOVE BW-LNAME                 TO PIBW-LNAME       (PIBW-ELE).
           MOVE BW-EMPPAYOT              TO PIBW-OTHER-INCOME(PIBW-ELE).
           MOVE BW-EMPPAYSR              TO PIBW-OTHER-INCOME-SOURCE
                                                             (PIBW-ELE).
           MOVE BW-SSNO                  TO PIBW-SSNO        (PIBW-ELE).
           MOVE BW-STATE                 TO PIBW-STATE       (PIBW-ELE).

      * LOAD 'STRING' INFO. 
           IF ( BW-ADR2 = SPACES )
              MOVE BW-ADR1               TO PIBW-ADDRESS     (PIBW-ELE)
           ELSE
              STRING BW-ADR1, " ", BW-ADR2
                   DELIMITED BY "  "   INTO PIBW-ADDRESS     (PIBW-ELE).

           STRING PIBW-ADDRESS(PIBW-ELE), " ",
                  BW-CITY, ", ", BW-STATE, " ", BW-ZIP
                   DELIMITED BY "  "   INTO PIBW-ADDRESS-CSZ (PIBW-ELE).

           STRING BW-CITY, ", ", BW-STATE, " ", BW-ZIP
                   DELIMITED BY "  "   INTO PIBW-CSZ         (PIBW-ELE).

           STRING BW-FNAME, " ", BW-LNAME, " ",
                  PIBW-ADDRESS-CSZ(PIBW-ELE)
                   DELIMITED BY "   "   INTO PIBW-NAME-ADDRESS-CSZ
                                                             (PIBW-ELE).

           STRING BW-FNAME, " ", BW-LNAME
                   DELIMITED BY "   "   INTO PIBW-NAME-FIRST-LAST
                                                             (PIBW-ELE).

           STRING BW-LNAME, ", ", BW-FNAME
                   DELIMITED BY "   "   INTO PIBW-NAME-LAST-FIRST
                                                             (PIBW-ELE).

           MOVE BW-ZIP                     TO PIBW-ZIP       (PIBW-ELE).

       PST-INFO-PIBW-LOAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - C H E C K S
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE CHECKS MADE FOR A GIVEN LOAN
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  LN-ACCTNO
      *           LN-PROCASH
      *    OUT :  CHECK-INFORMATION
      ******************************************************************
       PST-INFO-SETUP-CHECKS SECTION.

           PERFORM VARYING CHECK-SUB FROM 1 BY 1
                   UNTIL   CHECK-SUB > CHECK-MAX

                   MOVE ZEROES             TO CHECK-AMT(CHECK-SUB)
                   MOVE ZEROES             TO CHECK-NUM(CHECK-SUB)
                   MOVE SPACES             TO CHECK-TXT(CHECK-SUB)

           END-PERFORM.

           MOVE ZEROES                     TO CHECK-AMT-REMAINING.
           MOVE ZEROES                     TO CHECK-AMT-TO-BORROWER.
           MOVE 1                          TO CHECK-SUB.

           ADD  LN-PROCASH                 TO CHECK-AMT-TO-BORROWER.

      * CHECK FOR NETSPEND DEBIT CARD PROCEEDS
           IF BR-USE-DEBIT-CARDS = "Y"
              PERFORM PST-INFO-CHECK-FOR-DEBIT-CARD.

      * SETUP CHECKS USING THE CX-FILE

           MOVE LN-ORGBR TO CX-BRNO
                            QCX1-WBEG-BRNO
                            QCX1-WEND-BRNO.
           MOVE "LN"     TO CX-TYPE
                            QCX1-WBEG-TYPE
                            QCX1-WEND-TYPE.
           MOVE 0        TO CX-SEQNO
                            QCX1-WBEG-SEQNO.
           MOVE ALL "9"  TO QCX1-WEND-SEQNO.

           PERFORM START-CX1-FILE.

       PST-INFO-SETUP-CHK-NEXT-CX.

           PERFORM READ-CX1-FILE-NEXT.
           IF ( IO-BAD OR CX-BRNO NOT = LN-ORGBR ) AND            
              ( CHECK-SUB             = 1      ) AND
              ( CHECK-AMT-TO-BORROWER = ZEROES )
              GO TO PST-INFO-SETUP-CHECKS-TRC.

           IF ( IO-BAD ) OR ( CX-BRNO NOT = LN-ORGBR )            
              IF ( CHECK-SUB NOT = CHECK-MAX )
                 GO TO PST-INFO-SETUP-CHECKS-EXIT
              ELSE
                 GO TO PST-INFO-SETUP-CHKS-REMAIN.

           IF ( CX-REF NOT = LN-ACCTNO )
              GO TO PST-INFO-SETUP-CHK-NEXT-CX.

      * CHECK TO BORROWER 
           IF ( CX-BWPROC = "Y" )
              ADD CX-AMT TO CHECK-AMT-TO-BORROWER
              GO TO PST-INFO-SETUP-CHK-NEXT-CX.

      * CHECK TO OTHERS 
           MOVE CX-AMT   TO CHECK-AMT(CHECK-SUB).
           MOVE CX-CKNO  TO CHECK-NUM(CHECK-SUB).
           MOVE CX-PAYEE TO CHECK-TXT(CHECK-SUB).

           IF ( CHECK-SUB < CHECK-MAX )
              ADD 1      TO CHECK-SUB
           ELSE
              ADD CX-AMT TO CHECK-AMT-REMAINING.

           GO TO PST-INFO-SETUP-CHK-NEXT-CX.

      * SETUP CHECKS USING THE TRC-FILE

       PST-INFO-SETUP-CHECKS-TRC.

           MOVE LN-ORGBR    TO TC-BRNO
                               QTRC1-WBEG-BRNO
                               QTRC1-WEND-BRNO.
           MOVE LN-LOANDATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE
                                       QTRC1-WEND-KEY-DATE.
           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.

       PST-INFO-SETUP-CHK-NEXT-TRC.

           PERFORM READ-TRC1-FILE-NEXT.
           IF ((IO-BAD) OR (TC-BRNO NOT = LN-OWNBR)) AND  
              ( CHECK-SUB = 1 )
              GO TO PST-INFO-SETUP-CHECKS-EXIT.

           IF (( IO-BAD) OR (TC-BRNO NOT = LN-OWNBR))
              IF ( CHECK-SUB NOT = CHECK-MAX )
                 GO TO PST-INFO-SETUP-CHECKS-EXIT
              ELSE
                 GO TO PST-INFO-SETUP-CHKS-REMAIN.

      * STARTS WITH BRNO AND DATE, IF DATE DOESNT MATCH, YOU DIDNT FIND ANY
           IF TC-KEY-DATE NOT = LN-LOANDATE
              GO TO PST-INFO-SETUP-CHKS-REMAIN.

           IF ( TC-ACCTNO NOT = LN-ACCTNO )
              GO TO PST-INFO-SETUP-CHK-NEXT-TRC.

      * MUST CHECK IF IT HAS BEEN VOIDED !!
      * THEY COULD HAVE PRINTED, VOIDED, AND RE-ISSUED
           MOVE TC-BRNO TO CK-BRNO.
           MOVE TC-CKNO TO CK-NO.
           PERFORM READ-CK1-FILE.
           IF IO-FG NOT = 0
              GO TO PST-INFO-SETUP-CHK-NEXT-TRC
           ELSE
              IF CK-VOID = "Y" OR "R"
                 GO TO PST-INFO-SETUP-CHK-NEXT-TRC.

      * CHECK TO BORROWER  
           MOVE SPACES TO MESS.

      * YOU DO NOT HAVE THE BORROWER FIELDS CORRECTLY FILLED IN AT
      * THIS POINT, MUST READ AGAIN
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              STRING BW-FNAME, " ", BW-LNAME DELIMITED BY "   "
                                             INTO MESS
              IF ( TC-PAYEE = MESS )
                 ADD TC-AMT TO CHECK-AMT-TO-BORROWER
                 GO TO PST-INFO-SETUP-CHK-NEXT-TRC.

      * CHECK TO OTHERS 
           MOVE TC-AMT   TO CHECK-AMT(CHECK-SUB).
           MOVE TC-CKNO  TO CHECK-NUM(CHECK-SUB).
           MOVE TC-PAYEE TO CHECK-TXT(CHECK-SUB).

           IF ( CHECK-SUB < CHECK-MAX )
              ADD 1      TO CHECK-SUB
           ELSE
              ADD TC-AMT TO CHECK-AMT-REMAINING.

           GO TO PST-INFO-SETUP-CHK-NEXT-TRC.

       PST-INFO-SETUP-CHKS-REMAIN.

           IF ( CHECK-AMT-REMAINING NOT = CHECK-AMT(CHECK-SUB) )
              MOVE CHECK-AMT-REMAINING TO CHECK-AMT(CHECK-SUB)
              MOVE ZEROES              TO CHECK-NUM(CHECK-SUB)
              MOVE "OTHER"             TO CHECK-TXT(CHECK-SUB).

       PST-INFO-SETUP-CHECKS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - C U S T O M I Z E - C H E C K S
      *
      *-----------------------------------------------------------------
      *    DESC:  CUSTOMIZE THE NUMBER OF CHECKS FOR A GIVEN FORM
      *    FROM:  *** INDIVIDUAL PROGRAMS ***
      *    IN  :  CHECK-MAX-CUSTOMIZE
      *           CHECK-INFORMATION-TABLE
      *    OUT :  CHECK-INFORMATION-TABLE
      ******************************************************************
       PST-INFO-CUSTOMIZE-CHECKS SECTION.

           IF ( CHECK-MAX-CUSTOMIZE  = ZEROES    ) OR
              ( CHECK-MAX-CUSTOMIZE >= CHECK-MAX )
              GO TO PST-INFO-CUSTOM-CHKS-EXIT.

           COMPUTE CHECK-SUB = CHECK-MAX-CUSTOMIZE + 1.

           IF ( CHECK-AMT(CHECK-SUB) = ZEROES )
              GO TO PST-INFO-CUSTOM-CHKS-EXIT.

           PERFORM VARYING CHECK-SUB FROM CHECK-SUB BY 1
                   UNTIL   CHECK-SUB > CHECK-MAX

                   ADD  CHECK-AMT(CHECK-SUB          ) TO
                        CHECK-AMT(CHECK-MAX-CUSTOMIZE)

                   MOVE ZEROES TO CHECK-AMT(CHECK-SUB)
                   MOVE ZEROES TO CHECK-NUM(CHECK-SUB)
                   MOVE SPACES TO CHECK-TXT(CHECK-SUB)

           END-PERFORM.

           MOVE ZEROES  TO CHECK-NUM(CHECK-MAX-CUSTOMIZE).
           MOVE "OTHER" TO CHECK-TXT(CHECK-MAX-CUSTOMIZE).

       PST-INFO-CUSTOM-CHKS-EXIT.
           EXIT.

      ******************************************************************
       PST-INFO-CHECK-FOR-DEBIT-CARD SECTION.
      ******************************************************************
           PERFORM OPEN-LTD1-FILE.
           MOVE LN-OWNBR  TO LTD-BRNO.
           MOVE LN-ACCTNO TO LTD-ACCTNO.
           MOVE 1         TO LTD-SEQNO.
           PERFORM READ-LTD1-FILE.
           IF IO-FG NOT = 0
              GO TO PST-INFO-CHECK-FOR-DEBIT-CARD-EXIT.

           ADD LTD-DEBIT-CARD-AMT TO CHECK-AMT-TO-BORROWER.
       PST-INFO-CHECK-FOR-DEBIT-CARD-EXIT.
           PERFORM CLOSE-LTD1-FILE.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - C O D E S
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE CODES' INFORMATION (EXP. DESCRIPTIONS)
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  LN-REC
      *    OUT :  PST-INFO-CODE-WORKERS
      ******************************************************************
       PST-INFO-SETUP-CODES SECTION.

           MOVE SPACES TO PST-INFO-CODE-WORKERS.

      * COLLCD
           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF ( IO-BAD )
              MOVE "*NOT ON FILE*" TO CD-DESC.

           MOVE CD-DESC TO CODE-DESC-OF-COLLCD.

       PST-INFO-SETUP-CODES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - C O L L A T E R A L
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE COLLATERAL INFORMATION FOR A GIVEN LOAN
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  LN-ACCTNO
      *    OUT :  AUTO-INFORMATION-TABLE
      *           HHGS-INFORMATION-TABLE
      ******************************************************************
       PST-INFO-SETUP-COLLATERAL SECTION.

           MOVE SPACES TO AUTO-INFORMATION-TABLE
                          HHGS-INFORMATION-TABLE.

           PERFORM VARYING AUTO-SUB FROM 1 BY 1
                   UNTIL   AUTO-SUB > AUTO-MAX

                   MOVE ZEROES TO AUTO-MILEAGE (AUTO-SUB)
                                  AUTO-VALUE   (AUTO-SUB)
                                  AUTO-SELLPR  (AUTO-SUB)
                                  AUTO-MISC-AMT(AUTO-SUB)
                                  AUTO-VIN-CHG-DATE(AUTO-SUB)

           END-PERFORM.

           IF NOT ( PST-PRIMARY-FILE-LN )
              GO TO PST-INFO-SETUP-COLL-PC.

           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF ( IO-GOOD )
              MOVE LTC-AUTO(1) TO AUTO-INFO(1)
              MOVE LTC-AUTO(2) TO AUTO-INFO(2)
              MOVE LTC-AUTO(3) TO AUTO-INFO(3)
              MOVE LTC-HHGS(1) TO HHGS-INFO(1)
              MOVE LTC-HHGS(2) TO HHGS-INFO(2)
              MOVE LTC-HHGS(3) TO HHGS-INFO(3).

       PST-INFO-SETUP-COLL-PC.

           IF NOT ( PST-PRIMARY-FILE-PD )
              GO TO PST-INFO-SETUP-COLL-EXIT.

           MOVE "C"                        TO PC-ID.
           MOVE BRANCH                     TO PC-BRANCH.
           MOVE BORROWER-KEY               TO PC-SOC-SEC.
           PERFORM READ-PC1-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO WK-LT-RECORD
              MOVE ZEROES                  TO WK-MILEAGE-1
                                              WK-VALUE-1
                                              WK-SELLPR-1
                                              WK-MISC-AMT-1
                                              WK-VIN-CHG-DATE-1
                                              WK-MILEAGE-2
                                              WK-VALUE-2
                                              WK-SELLPR-2
                                              WK-MISC-AMT-2
                                              WK-VIN-CHG-DATE-2
           ELSE
              MOVE PC-LTC-HHGS(1) TO WK-HHGS-1
              MOVE PC-LTC-HHGS(2) TO WK-HHGS-2
              MOVE PC-LTC-HHGS(3) TO WK-HHGS-3
              MOVE PC-LTC-AUTO(1) TO WK-AUTO-1
              MOVE PC-LTC-AUTO(2) TO WK-AUTO-2
              MOVE PC-LTC-AUTO(3) TO WK-AUTO-3.

           MOVE WK-AUTO-1                  TO AUTO-INFO(1).
           MOVE WK-AUTO-2                  TO AUTO-INFO(2).
           MOVE WK-AUTO-3                  TO AUTO-INFO(3).
           MOVE WK-HHGS-1                  TO HHGS-INFO(1).
           MOVE WK-HHGS-2                  TO HHGS-INFO(2).
           MOVE WK-HHGS-3                  TO HHGS-INFO(3).

       PST-INFO-SETUP-COLL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - F E E S
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE FECCODE AMOUNTS AND DESCRIPTIONS
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  LN-FEECODE(1~10)
      *    OUT :  PST-INFO-FEE-AMT-WORKERS
      *           PST-INFO-FEE-TXT-WORKERS
      ******************************************************************
       PST-INFO-SETUP-FEES SECTION.

           MOVE ZEROES TO FEE-SUB.
           MOVE ZEROES TO PST-INFO-FEE-AMT-WORKERS.
           MOVE SPACES TO PST-INFO-FEE-TXT-WORKERS.

       PST-INFO-SETUP-FEES-LOOP.

           ADD 1 TO FEE-SUB.

           IF ( FEE-SUB > FEE-LN-MAX )
              GO TO PST-INFO-SETUP-FEES-EXIT.

           IF ( LN-FEECODE(FEE-SUB) = SPACES )
              GO TO PST-INFO-SETUP-FEES-LOOP.

           MOVE LN-FEECODE(FEE-SUB) TO FEE-CODE-ALPHANUMERIC.

           IF ( FEE-CODE-ALPHANUMERIC = SPACES )
              GO TO PST-INFO-SETUP-FEES-LOOP.

           IF ( FEE-CODE-1X NOT NUMERIC ) AND
              ( FEE-CODE-2X NOT NUMERIC )
              GO TO PST-INFO-SETUP-FEES-LOOP.

           IF ( FEE-CODE-1X = SPACES )
              MOVE "0" TO FEE-CODE-1X
           ELSE
           IF ( FEE-CODE-2X = SPACES )
              MOVE FEE-CODE-1X TO FEE-CODE-2X
              MOVE "0"         TO FEE-CODE-1X.

           IF ( FEE-CODE-NUMERIC NOT NUMERIC ) OR
              ( FEE-CODE-NUMERIC   = ZEROES  )
              GO TO PST-INFO-SETUP-FEES-LOOP.

           MOVE "GC"                TO CDB-TYPE.
           MOVE BR-NO               TO CDB-BRANCH.
           MOVE LN-FEECODE(FEE-SUB) TO CDB-CODE.
           PERFORM READ-CDB1-FILE.
           IF ( IO-BAD )
              MOVE "*NOT ON FILE*  " TO CDB-DESC.

           MOVE CDB-DESC           TO FEE-TXT(FEE-CODE).
           ADD  LN-FEEAMT(FEE-SUB) TO FEE-AMT(FEE-CODE).

           GO TO PST-INFO-SETUP-FEES-LOOP.

       PST-INFO-SETUP-FEES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - I N S U R A N C E
      *
      *-----------------------------------------------------------------
      *    DESC:  GET ALL INSURANCE INFORMATION FOR ALL INSURANCES
      *            MAXIMUM COVERAGE & TERMS
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  LN-INSCOVR(*)
      *           LN-INSPREM(*)
      *           LN-INSTYPES
      *           LN-INSURED
      *           SP-INSMAXCHGABL(*)
      *    OUT :  INS-MAXCOVR(*)
      *           INS-TERM(*)
      ******************************************************************
       PST-INFO-SETUP-INSURANCE SECTION.

           PERFORM VARYING INS-SUB FROM 1 BY 1 UNTIL INS-SUB > INS-MAX

                   MOVE ZEROES TO INS-TERM   (INS-SUB)
                   MOVE ZEROES TO INS-MAXCOVR(INS-SUB)

           END-PERFORM.

      * CREDIT LIFE
           MOVE LN-INSCOVR(1) TO INS-MAXCOVR-CL.

           IF ( LN-INSURED NOT = "J"               ) AND
              ( LN-INSTYPES-DL                     ) AND
              ( LN-INSCOVR(1) > SP-INSMAXCHGABL(1) )
              MOVE SP-INSMAXCHGABL(1) TO INS-MAXCOVR-CL.

           IF ( LN-INSURED NOT = "J"               ) AND
              ( LN-INSTYPES-LL                     ) AND
              ( LN-INSCOVR(1) > SP-INSMAXCHGABL(3) )
              MOVE SP-INSMAXCHGABL(3) TO INS-MAXCOVR-CL.

           IF ( LN-INSURED = "J"                   ) AND
              ( LN-INSTYPES-DL                     ) AND
              ( LN-INSCOVR(1) > SP-INSMAXCHGABL(2) )
              MOVE SP-INSMAXCHGABL(2) TO INS-MAXCOVR-CL.

           IF ( LN-INSURED = "J"                   ) AND
              ( LN-INSTYPES-LL                     ) AND
              ( LN-INSCOVR(1) > SP-INSMAXCHGABL(4) )
              MOVE SP-INSMAXCHGABL(4) TO INS-MAXCOVR-CL.

           PERFORM VARYING INS-SUB FROM 1 BY 1 UNTIL INS-SUB > INS-MAX

                   IF ( LN-INSPREM(INS-SUB) NOT = ZEROES )
                      MOVE INS-SUB      TO ITRM-SUB
                      PERFORM INS-TERM-CALCULATION
                      MOVE ITRM-INSTERM TO INS-TERM(INS-SUB)
                   END-IF

           END-PERFORM.

       PST-INFO-SETUP-INS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - L O A N
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE INFORMATION NEEDED FOR THE LOAN
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  LN-REC
      *    OUT :  PRIOR-ACCTNO-PAYOFF
      ******************************************************************
       PST-INFO-SETUP-LOAN SECTION.

           MOVE ZEROES TO PRIOR-ACCTNO-PAYOFF.

           COMPUTE PRIOR-ACCTNO-PAYOFF = LN-PRBAL
                                       + LN-PRINTDUE
                                       - LN-PRFINCHGRB
                                       - LN-PRINSRB.

           MOVE LN-PRLNNO TO PRIOR-ACCTNO.

       PST-INFO-SETUP-LOAN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - I N F O - S E T U P - M O N - D I S - B E N
      *
      *-----------------------------------------------------------------
      *    DESC:  GET THE MONTHLY DISABILITY BENEFIT
      *    FROM:  PST-INFO-ROUTINE
      *    IN  :  LN-INSTYPES
      *           LN-MAINTFEE
      *           LN-REGPYAMT
      *           SP-INSMAXBENIF(5)
      *    OUT :  MONTHLY-DISABILITY-BENEFIT
      ******************************************************************
       PST-INFO-SETUP-MON-DIS-BEN SECTION.

           MOVE ZEROES TO MONTHLY-DISABILITY-BENEFIT.

           IF ( NOT LN-INSTYPES-AH )
              GO TO PST-INFO-SETUP-MDB-EXIT.

           COMPUTE MONTHLY-DISABILITY-BENEFIT = LN-REGPYAMT
                                              - LN-MAINTFEE.

           IF ( MONTHLY-DISABILITY-BENEFIT > SP-INSMAXBENIF(5) )
              MOVE SP-INSMAXBENIF(5) TO MONTHLY-DISABILITY-BENEFIT.

       PST-INFO-SETUP-MDB-EXIT.
           EXIT.
