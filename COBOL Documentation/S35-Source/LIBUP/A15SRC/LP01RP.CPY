      * COPYMEMBER: LIBLP/LP01RP
      **************************************************************************
      *
      *   IDENTIFICATION: LIBLP/LP01RP
      *
      *   DESCRIPTION   : REVOLVING PROGRAM CODE FILE          RSZ: 0100
      *
      *-------------------------------------------------------------------------
      * REV:
      *  KEC 110398 ***NEW***                                   RSZ: 0100
      *  JTG 071399 ADDED RP-RATE-INDEX-ADJ-CODE-VALID "F" FIRST DAY OF CYCLE
      *  JTG 000214 ADDED RP-NO-PYMT-REQ-SWING-DAYS
      *                   RP-CONTGNT-INT-FRMLA
      *                   RP-CONTGNT-REVOL-RVPROG
      *  JTG 020626 ADDED RP-RATE-ALLOW-OVERRIDE                  LENDMARK #1841
      **************************************************************************
       01  RP-REC.
           03  RP1-KEY.
               05  RP-PROG                       PIC X(2).
               05  RP-LAWCODE                    PIC 9(2).

      *=================================================================
           03  RP-DESC                           PIC X(30).

           03  RP-AVG-DAILY-BAL                  PIC X(1).
               88  RP-AVG-DAILY-BAL-VALID            VALUE "A".

           03  RP-BASE-CODE                      PIC X(1).
               88  RP-BASE-CODE-DAYS                 VALUE "D".
               88  RP-BASE-CODE-MONTHS               VALUE "M".

           03  RP-EFFDATE                        PIC 9(6)        COMP-3.
           03  RP-EXPDATE                        PIC 9(6)        COMP-3.

           03  RP-FINCHG-CHGABLE-CODE            PIC X(1).
               88  RP-FINCHG-CHGABLE-CODE-VALID      VALUE "A".

           03  RP-MONTHS-DAYS                    PIC 9(4)        COMP-3.
           03  RP-CYCLES-AT-REDUCED-PYMT         PIC 9(2).
           03  RP-CYCLES-NO-PYMT-REQ             PIC 9(2).
           03  RP-MONTHS-TO-INTEREST-START       PIC 9(2).

           03  RP-PYMT-FRMLA                     PIC X(1).
               88  RP-PYMT-FRMLA-VALID           VALUE "A" "B".

           03  RP-RATE                           PIC 999V999     COMP-3.

           03  RP-RATE-FRMLA                     PIC X(1).
               88  RP-RATE-FRMLA-VALID           VALUE "F" "S" "1" "2".

           03  RP-RATE-INDEX-ADJ-CODE            PIC X(1).
               88  RP-RATE-INDEX-ADJ-CODE-VALID      VALUE " " "B" "F"
                                                           "L" "P".

           03  RP-REDUCED-PYMT                   PIC 9(6)V99     COMP-3.

           03  RP-YEAR-TYPE                      PIC 9(4)        COMP-3.
               88  RP-YEAR-TYPE-VALID                VALUE 358 360 365
                                                           367 999.

           03  RP-MIN-PURCHASE                   PIC 9(4)V99     COMP-3.

      *-----------------------------------------------------------------
      *    MOVED FROM SP-RECORD 02/01/00
           03  RP-PYMT-EXCEPTION                 PIC X(1).
               88  RP-PYMT-EXCEPTION-VALID    VALUE " ".
     
           03  RP-PYMT-MINIMUM                   PIC 9(4)V99  COMP-3.
           03  RP-PYMT-MAXIMUM                   PIC 9(4)V99  COMP-3.
           03  RP-PYMT-PERCENT                   PIC 999V999  COMP-3.

           03  RP-PYMT-ROUND-CODE                PIC X(1).
               88  RP-PYMT-ROUND-CODE-VALID   VALUE "C" "R" "T".

           03  RP-NO-PYMT-REQ-SWING-DAYS         PIC 9(2).
           03  RP-CONTGNT-INT-FRMLA              PIC X.
               88  RP-CONTGNT-INT-ROLL-TO-PRIN   VALUE "P".
           03  RP-CONTGNT-REVOL-RVPROG           PIC XX.
      *-----------------------------------------------------------------
           03  RP-RATE-ALLOW-OVERRIDE            PIC X.
           03  FILLER                            PIC X(7).
