      * COPYMEMBER: LIBLP/LP01CL
      ******************************************************************
      *
      * DESCRIPTION: CAN LOAN FILE (CL)
      * RECORD SIZE: 3450 BYTES (CL-REC)
      *              3350 BYTES (CL-PD-REC)
      * RECORD KEY : CL-GROUP-LOC
      *              CL-CAN-NO
      *
      * ***NOTE*** : CL-PD-REC & CL-PD-SIZE MUST = PD-REC (LIBLP/LP01PD)
      *
      *=================================================================
      * REV:
      * JTG 060597 CREATED FILE
      * JTG 062397 CHANGED TO 4 DIGIT GROUP
      * JTG 082097 INCREASED CL-PD-REC FROM 1830 TO 1889
      * JTG 112097 EXPANDED CL-LN-REC BY 230 BYTES
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLM 160615 INCREASED CL-PD-REC FROM 2289 TO 3308
      * KEC 161004 ADDED CL-LTOUCH-DATE.
      *            INCREASED FILLER TO X(55).
      *            CHANGED   CL-PD-REC FROM 3308 TO 3200 (CL-PD-SIZE).
      *            CHANGED   CL-REC    FROM 3368 TO 3300 (CL-SIZE).
      *
      * KEC 2017.0615 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          IN REVIEW OF A30-TO-A15 CHANGES, NOTICED THAT THE
      *          CL-PD-REC WAS SMALLER THAN PD-REC.
      *          CHANGED CL-PD-REC FROM 3200 TO 3350 (CL-PD-SIZE).
      *          CHANGED CL-REC    FROM 3300 TO 3450 (CL-SIZE).
      *
      ******************************************************************
      *
      * ***NOTE*** : CL-PD-REC & CL-PD-SIZE MUST = PD-REC (LIBLP/LP01PD)
      *
      ******************************************************************
       78  CL-SIZE                         VALUE 3450.
       78  CL-PD-SIZE                      VALUE 3350.
       01  CL-REC.
           03  CL1-KEY.
               05  CL-GROUP-ID.
                   07  CL-GROUP-LOC    PIC 9(4).
               05  CL-CAN-NO           PIC XX.
      *    03  CL2-KEY:
      *        05  CL-GROUP-ID.
      *            07  CL-GROUP-LOC    PIC 9(4).
           03  CL-LTOUCH-DATE          PIC 9(08)     COMP-3.
           03  CL-DESC                 PIC X(33).

           03  CL-GIFTNET              PIC X.
           03  FILLER                  PIC X(55).
      *    --------------------------------------------------
           03  CL-PD-REC               PIC X(OCL-PD-SIZE).

