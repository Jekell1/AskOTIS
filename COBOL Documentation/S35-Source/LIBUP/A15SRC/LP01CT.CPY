      * COPYMEMBER: LIBLP/LP01CT
      **************************************************************************
      *
      *   DESCRIPTION:  CODE FILE TRAILER RECORD
       78  CT-SIZE                               VALUE 242.
      *
      *-------------------------------------------------------------------------
      *   KEY - TYPE, CODE, FILLER                             
      *    
      *       TYPE  :
      *        GC     -  FEE CODES
      *
      *       CODE  : TYPE DEPENDENT
      *       FILLER: TYPE DEPENDENT
      *
      *-------------------------------------------------------------------------
      * REV:
      * JTG 030422 *NEW* WCBC FEE GL ENTRY FILE                       WORLD #323
      **************************************************************************
       01  CT-REC.
           03  CT1-KEY.
               05  CT-TYPE                       PIC X(2).
               05  CT-CODE                       PIC X(3).
               05  CT-FILLER                     PIC X(3).

           03  FILLER                            PIC X(234).

      *-----------------------------------------------------------------
      ******************************************************************
      *
      *    GC -  FEE CODES
      *
      ******************************************************************
       01  CT-FEEREC.
           03  CT-FEEKEY.
               05  CT-FEETYPE                    PIC X(2).
               05  CT-FEEBR                      PIC 9(4).
               05  CT-FEECD                      PIC X(2).

      *-----------------------------------------------------------------
           03  CT-GLNO-TBL                       OCCURS 12.
               05  CT-ACCT-DB                    PIC 9(11)   COMP-3.
               05  CT-ACCT-CR                    PIC 9(11)   COMP-3.
               05  CT-GLAMT                      PIC 9(8)V99 COMP-3.
           03  FILLER                            PIC X(18).

