      * COPYMEMBER: LIBLP/LP01RL
      ******************************************************************
      *
      *   IDENTIFICATION: LIBLP/LP01RL                        BYTES = 33
      *
      *   DESCRIPTION   : REVOLVING PAYMENT LINK FILE RECORD
      *
      *-----------------------------------------------------------------
      *   KEY 1 -   RL-ACCTNO, RL-PY-RT-SEQNO, RL-PU-RT-SEQNO
      *   KEY 2 -   RL-ACCTNO, RL-PU-RT-SEQNO, RL-PY-RT-SEQNO
      *
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      * REV:
      * JTG 121698 ***NEW***                                 BYTES = 33 
      * JTG 121798 ADDED KEY2 RL-ACCTNO, RL-PU-RT-SEQNO, RL-PY-RT-SEQNO
      *
      ******************************************************************
       78  RL-SIZE                                            VALUE 33.
      *-----------------------------------------------------------------
       01  RL-REC.
           03  RL1-KEY.
               05  RL-ACCTNO               PIC 9(8)       COMP-3.
      *       LINK FROM 'RT-PAYMENT-RECORD'
               05  RL-PY-RT-SEQNO          PIC 9(5)       COMP-3.
      *       LINK TO 'RT-PURCHASE-RECORD'
               05  RL-PU-RT-SEQNO          PIC 9(5)       COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  RL-PRIOR-BAL                PIC S9(7)V99   COMP-3.
           03  RL-APPLIED                  PIC S9(7)V99   COMP-3.
           03  RL-ADB-THRUDAYS             PIC 99         COMP-3.
  
           03  FILLER                      PIC X(10).

      *=================================================================

