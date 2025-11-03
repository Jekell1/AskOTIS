      * COPYMEMBER: LIBCL/FEEADDW
      ****************************************************
      *    FEEADDW       
      *    WORKERS FOR CL/FEEADD.C
      *
      * REV: 
      *  20241111 BAH NEW FOR CL/FEEADD, VERIFY AD&D CAN BE SOLD #1685 S35Q-144
      ****************************************************
       01  FEEADD-WORKERS.
           03  FEEADD-INS-FEECODE         PIC XX.
           03  FEEADD-SSNO                PIC 9(9).
           03  FEEADD-TRANS-DATE          PIC 9(8).

           03  FEEADD-CAN-SELL            PIC X.
