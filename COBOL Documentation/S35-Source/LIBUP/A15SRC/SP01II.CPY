      * COPYMEMBER: LIBSP/SP01II
      * 012799 BAH CHANGES AS PER IRS
      * 021217 BAH ADDED II-CONTACT-EMAIL (PER NEW IRS FIELD)
      * 12/13/2016 BAH ADDED II-TAX-YEAR, II-CURRENT-LAST, II-LASER-FORM FOR THE 1098 FORM TO KNOW WHAT TO PRINT PR#5221

       01  II-REC.
           03  II-EMPLOYER-ID            PIC 9(9) COMP-3.
           03  II-NAME-CONTROL-NO        PIC X(4).
           03  II-TRANS-CONTROL-CODE     PIC X(5).
           03  II-FOREIGN-CORP           PIC X.
           03  II-TRANSFER-AGENT-FG      PIC X.
           03  II-PAYER-NAME             PIC X(40).
           03  II-SECOND-PAYER           PIC X(40).
           03  II-PAYER-ADDRESS          PIC X(40).
           03  II-PAYER-CITY             PIC X(29).
           03  II-PAYER-STATE            PIC X(2).
           03  II-PAYER-ZIP              PIC X(9).
           03  II-PAYER-PHONE            PIC 9(10).
           03  II-PAYER-EXT              PIC X(5).
           03  II-CONTACT                PIC X(40).
           03  II-CONTACT-EMAIL          PIC X(45).
           03  II-TAX-YEAR               PIC 9(4).
           03  II-CURRENT-LAST           PIC X.
           03  II-LASER-FORM             PIC X.
           03  FILLER                    PIC X(54).

