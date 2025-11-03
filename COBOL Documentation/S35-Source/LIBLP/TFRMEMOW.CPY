      * COPYMEMBER: LIBLP/TFRMEMOW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/TFRMEMOW
      *
      * DESCRIPTION   :  WORKERS FOR CREATING A MEMO FOR ACCOUNTS
      *                  TRANSFER TO ANOTHER BRANCH.
      *
      * PROGRAMS USED :  LP/BTPURG
      *                  LP/BULKG2   [CREATING MEMO]
      *                  LP/BULKT2   [CREATING MEMO]
      *                  LP/CURBAL
      *                  LP/LNTIM6
      *                  LP/SSUPD
      *                  SP/AMPROP
      *                  SP/BNKOFA
      *                  SP/COLINS
      *                  SP/COLPRO
      *                  SP/ILUEXT
      *                  SP/KISTLR
      *                  SP/SICINS
      *                  SP/SICREG
      *                  SP/SICRGM
      *
      *=================================================================
      *
      * EXAMPLE MEMOS CREATED [LP/BULKG2 & LP/BULKT2]
      * ---------------------------------------------
      *
      *          -BRNO-     -BRNO-   -ACCTNO/CURBAL-   -ACCTNO-
      * TFR FR BR #### TO BR ####                                   [BM]
      * TFR TO BR #### FR BR ####, RE: 12345678 ---> 12345678       [BM]
      * TFR TO BR #### FR BR ####, FOR $ZZZZZ9.99 ---> 12345678     [LM]
      * TFR FR BR #### TO BR ####, FOR $ZZZZZ9.99 <--- 12345678     [LM]
      *
      *=================================================================
      * REV:
      * KEC 2017.0612 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *          ***NEW*** CREATED WHILE WORKING ON THE
      *          A30-TO-15 PROGRAM (UP/UL1530).  WHILE REVIEWING
      *          A30 LOAN (LN) FILE CHANGES, I FOUND WHERE A MEMO
      *          CREATED WITHIN BULK TRANSFERS (LP/BULKG2 & LP/BULKT2)
      *          ARE USING THE NEW ACCOUNT NUMBER (8 BYTES).
      *          BUT NUMEROUS PROGRAMS THAT CHECK/USE THE MEMOS LOOKS
      *          AT THE OLD ACCOUNT NUMBER (6 BYTES).
      *          THE UPGRADE PROGRAM KEPT THE OLD (A15) MEMOS THE SAME
      *          AS IS WHICH THE ACCOUNT NUMBER WAS 6 BYTES.
      *          THESE NEW WORKERS ARE USED TO CREATE A STANDARD IN
      *          CREATING AND CHECKING/USING THE ACCOUNT NUMBER OF
      *          8 BYTES, AS WELL AS CONVERTING A15 MEMOS.
      *
      ******************************************************************
       01  TFRMEMO-MEMO                PIC X(60)     VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BASE FORMAT - USED FOR IDENTIFYING WHAT TYPE OF MEMO.
      * TFR FR BR #### TO BR ####                                   [BM]
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FILLER                      REDEFINES TFRMEMO-MEMO.
           03  TFRMEMO-PRIMARY         PIC X(09).
               88  TFRMEMO-PRIMARY-FROM              VALUE "TFR FR BR".
               88  TFRMEMO-PRIMARY-TO                VALUE "TFR TO BR".
           03  FILLER                  PIC X(01).
           03  TFRMEMO-BRNO1           PIC 9(04).
           03  FILLER                  PIC X(01).
           03  TFRMEMO-SECONDARY       PIC X(05).
               88  TFRMEMO-SECONDARY-FROM            VALUE "FR BR".
               88  TFRMEMO-SECONDARY-TO              VALUE "TO BR".
           03  FILLER                  PIC X(01).
           03  TFRMEMO-BRNO2           PIC 9(04).
           03  FILLER                  PIC X(35).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TFR TO BR #### FR BR ####, RE: 12345678 ---> 12345678       [BM]
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FILLER                      REDEFINES TFRMEMO-MEMO.
           03  FILLER                  PIC X(25).
           03  TFRMEMO-BM-RE           PIC X(06).
               88  TFRMEMO-BM-RE-VALID               VALUE ", RE: ".
           03  TFRMEMO-BM-ACCTNO1      PIC 9(08).
           03  FILLER                  PIC X(01).
           03  TFRMEMO-BM-DIRECTION    PIC X(04).
               88  TFRMEMO-BM-DIRECTION-VALID        VALUE "--->".
           03  FILLER                  PIC X(01).
           03  TFRMEMO-BM-ACCTNO2      PIC 9(08).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TFR TO BR #### FR BR ####, FOR $ZZZZZ9.99 ---> 12345678     [LM]
      * TFR FR BR #### TO BR ####, FOR $ZZZZZ9.99 <--- 12345678     [LM]
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FILLER                      REDEFINES TFRMEMO-MEMO.
           03  FILLER                  PIC X(25).
           03  TFRMEMO-LM-FOR          PIC X(07).
               88  TFRMEMO-LM-FOR-VALID              VALUE ", FOR $".
           03  TFRMEMO-LM-CURBAL       PIC ZZZZZ9.99.
           03  TFRMEMO-LM-CURBALX      REDEFINES TFRMEMO-LM-CURBAL
                                       PIC X(09).
           03  FILLER                  PIC X(01).
           03  TFRMEMO-LM-DIRECTION    PIC X(04).
               88  TFRMEMO-LM-DIRECTION-VALID        VALUE "--->"
                                                           "<---".
           03  FILLER                  PIC X(01).
           03  TFRMEMO-LM-ACCTNO       PIC 9(08).

