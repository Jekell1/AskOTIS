      * COPYMEMBER: LIBLP/LP01SU
      **************************************************
      *   STATE CODE TRAILER FILE     300 BYTES    
      *         
      * REV:
      * AMM 2014.0916 PR#356 ADDED TRAILER FILE ***NEW***
      * BAH 2017.0719 ADDED SU-MAX-LOAN-AVAIL WORLD PR#1260
      * BAH 2018.0913 ADDED SU-MAX-TAX-LOAN-AMT AND SU-NSF-FEE-AMT, #1334
      **************************************************
       78  SU-SIZE                     VALUE 300.
       01  SU-REC.
           03  SU1-KEY.
               05  SU-CODE             PIC XX.
           03  SU-GEOGRAPHIC-CODE      PIC 9(04).
           03  SU-GEOGRAPHIC-CODEX     REDEFINES SU-GEOGRAPHIC-CODE
                                       PIC X(04).                                      
           03  SU-MAX-LOAN-AVAIL       PIC 9(6)V99 COMP-3.
           03  SU-MAX-TAX-LOAN-AMT     PIC S9(7)V99 COMP-3.
           03  SU-NSF-FEE-AMT          PIC S9(5)V99 COMP-3.          
           03  FILLER                  PIC X(280).
