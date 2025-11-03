      * COPYMEMBER: LIBLP/LP01CL
      ********************************************************************
      *                   CAN LOAN FILE
       78  CL-SIZE                         VALUE 2349.
       78  CL-PD-SIZE                      VALUE 2289.
      *                                                
      * REV:
      *  JTG 060597 CREATED FILE
      *  JTG 062397 CHANGED TO 4 DIGIT GROUP
      *  JTG 082097 INCREASED CL-PD-REC FROM 1830 TO 1889
      *  JTG 112097 EXPANDED CL-LN-REC BY 230 BYTES 
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *
      ********************************************************************
       01  CL-REC.
           03  CL1-KEY.
               05  CL-GROUP-ID.
                   07  CL-GROUP-LOC    PIC 9(4).
               05  CL-CAN-NO           PIC XX.
      *    03  CL2-KEY:
      *        05  CL-GROUP-ID.
      *            07  CL-GROUP-LOC    PIC 9(4).
           03  CL-DESC                 PIC X(33).

           03  CL-GIFTNET              PIC X.
           03  FILLER                  PIC X(20).
      *    --------------------------------------------------
           03  CL-PD-REC               PIC X(OCL-PD-SIZE).

