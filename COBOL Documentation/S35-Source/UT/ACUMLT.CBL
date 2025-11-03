       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ACUMLT.
       AUTHOR.        JKC.
       DATE-WRITTEN.  06-03-2022.
      ******************************************************************
      *
      * IDENTIFICATION: UT/ACUMLT.CBL
      *
      * DESCRIPTION   : ACU MEMORY LEAK TEST
      *
      * EXECUTION     : Command Line
      * exec runcbl -s -c /$REL/SH/xcblconfig +e /tmp/acu_tclp -x ACUMLT
      *
      * SPECIAL NOTE  : The execution requires the use of an external
      *                 bash script:
      *
      *    move "sh /usr/local/bin/acumlt.sh" to system-buf-cmd.
      *
      *=================================================================
      * Read through LNFILE for X many times while watching & recording
      * memory usage using the Linux `free` command.
      *
      * 1. Loop thru LNFILE (Declare cursor and fetch)
      *    A. Read matching BWFILE (1 to 4 times)
      *
      *=================================================================
      *
      * /usr/local/bin/acumlt.sh
      * ------------------------
      *
      * # MAIN-MODULE.
      * 
      *  cd /usr/tmp
      * 
      *  if [ $? -ne 0 ]; then
      *     exit 9
      *  fi
      * 
      *  if [ -z $1 ]; then
      *     exit 9
      *  fi
      * 
      * #==================================================================
      * # Initialization
      * #==================================================================
      *  lineSingle=`printf '%.s-' {1..80}`     #80 character line of =====
      * 
      *  outputFile=$1
      * 
      * #==================================================================
      * # Find PID for runcbl
      * # 1. Use `ps -fu` to get runcbl process
      * # 2. Use sed to remove the grep process to find runcbl process
      * # 3. Use cat to extract PID from runcbl process
      * #==================================================================
      *  workFile=/usr/tmp/acumlt-wk-$$              #Create temp work file
      *  ps -fu | grep " runcbl " > $workFile        #Get runcbl processes
      *  sed -i -r '/grep/d' $workFile               #Remove grep lines
      *  runcblPID=`cat $workFile | cut -c10-14`     #Extract PID
      *  rm $workFile                                #Remove temp work file
      * 
      * #==================================================================
      * # Get runcbl process memory usage by using `pmap` then
      * # getting the total line (tail -n 1) then getting the memory usage (awk).
      * #==================================================================
      *  if [ -z "$runcblPID" ]; then                            #Check if no PID
      *     runcblPID="N/A"
      *     runcblMemory="N/A"
      *  else
      *     runcblMemory=`pmap $runcblPID | tail -n 1 | awk '/[0-9]K/{print $2}'`
      *  fi
      * 
      * #==================================================================
      * # Add to last output file with time stamp and memory usage
      * #==================================================================
      *  echo "Process Memory [$runcblPID]: $runcblMemory" >> $outputFile
      *  echo "`free`"                                     >> $outputFile
      *  echo "$lineSingle"                                >> $outputFile
      *  exit 0
      *
      *=================================================================
      * REV:
      * JKC 2022-0603 ***NEW***
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/ACUMLT S35 03/31/23-09:23:10 >".

      *=================================================================
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  sqlserver     pic x(64) value spaces.
       01  sqluser       pic x(32) value spaces.
       01  sqlpassword   pic x(32) value spaces.

       01  C-RECORD.
           05  C-NUMBER                            PIC S9(3) COMP-5.
           05  C-FIRST-NAME                        PIC X(80).
           05  C-LAST-NAME                         PIC X(80).
           05  C-BIRTHDAY                          PIC X(10).
           05  C-INFO                              PIC X(10).

       01  num-rows                            pic s9(5) comp-5.
       01  ret-code                            pic s9(4) comp-5.
       01  dsn-name                            pic x(20).
      *01  userid                              pic x(8).
       01  userid                              pic x(16).
       01  passwd.
      *kc  49 passwd-length                    PIC s9(4) comp-5 value 0.
           49 passwd-name                      PIC x(18).

       01  QBW-REC.
           03  QBW-OWNBR                              PIC 9(4).
           03  QBW-SSNO                               PIC 9(9).
           03  QBW-LNAME                              PIC X(35).
           03  QBW-FNAME                              PIC X(35).
           03  QBW-MNAME                              PIC X(35).
           03  QBW-NICKNAME                           PIC X(35).
           03  QBW-GENERATION                         PIC X(7).
           03  QBW-ADR1                               PIC X(30).
           03  QBW-ADR2                               PIC X(30).
           03  QBW-CITY                               PIC X(30).
           03  QBW-STATE                              PIC XX.
           03  QBW-ZIP                                PIC X(10).
           03  QBW-LTOUCH-DATE                        PIC X(10).
           03  QBW-REFDATE                            PIC X(10).
           03  QBW-BDATE                              PIC X(10).
           03  QBW-PHONE                              PIC 9(10).
           03  QBW-CELL-PHONE                         PIC 9(10).
           03  QBW-NATIONALITY                        PIC X(8).
           03  QBW-GENDER                             PIC X.
           03  QBW-DEPEND                             PIC 99.
           03  QBW-RENTCD                             PIC X(4).
           03  QBW-TIMEADDR                           PIC X(4).
           03  QBW-CREDITCD                           PIC X(4).
           03  QBW-STRTGCD                            PIC X(6).
           03  QBW-DWELLCD                            PIC XX.
           03  QBW-COUNTY                             PIC X(4).
           03  QBW-ORIGIN                             PIC X(4).
           03  QBW-CENSUSCD                           PIC X(10).
           03  QBW-ORGSLSMN                           PIC 9(4).
           03  QBW-REPCODE                            PIC X(4).
           03  QBW-SPSSNO                             PIC 9(9).
           03  QBW-SPLNAME                            PIC X(35).
           03  QBW-SPFNAME                            PIC X(35).
           03  QBW-SP-BDATE                           PIC X(10).
           03  QBW-EMPLOYER                           PIC X(35).
           03  QBW-EMPLPOS                            PIC X(24).
           03  QBW-EMPPHONE                           PIC 9(10).
           03  QBW-EMPEXT                             PIC 9(4).
           03  QBW-EMPYEARS                           PIC 99.
           03  QBW-EMPPAY                             PIC 9(7)V99.
           03  QBW-EMPPAYOT                           PIC 9(7)V99.
           03  QBW-EMPPAYSR                           PIC X(8).
           03  QBW-EMPADDR1                           PIC X(30).
           03  QBW-EMPADDR2                           PIC X(30).
           03  QBW-EMPCITY                            PIC X(30).
           03  QBW-EMPSTATE                           PIC XX.
           03  QBW-EMPZIP                             PIC X(10).
           03  QBW-WORKHRS                            PIC X(9).
           03  QBW-LOANS                              PIC 99.
           03  QBW-INTYTD                             PIC S9(7)V99.
           03  QBW-INTLYR                             PIC S9(7)V99.
           03  QBW-LIASMLN                            PIC S9(7)V99.
           03  QBW-LIAOTH                             PIC S9(7)V99.
           03  QBW-LASTPODATE                         PIC X(10).
           03  QBW-PRLOANS                            PIC 9(3).
           03  QBW-RE-INTYTD                          PIC S9(7)V99.
           03  QBW-RE-INTLYR                          PIC S9(7)V99.
           03  QBW-CREDIT-LIMIT                       PIC 9(7)V99.
           03  QBW-CREDIT-SCORE                       PIC 9(4).
           03  QBW-CRED-SCORE-1                       PIC 9(4).
           03  QBW-CRED-SCORE-2                       PIC 9(4).
           03  QBW-CRED-SCORE-3                       PIC 9(4).
           03  QBW-CRED-SCORE-4                       PIC 9(4).
           03  QBW-BNKRPT-SCORE-1                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-2                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-3                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-4                     PIC 9(4).
           03  QBW-LAST-FORM-NO                       PIC 9(4).
           03  QBW-LAST-FORM-DATE                     PIC X(10).
           03  QBW-PRIOR-RN-FOR10-DATE                PIC X(10).
           03  QBW-LAST-RN-FOR10-DATE                 PIC X(10).
           03  QBW-ADR-PHONE-CHG-DATE                 PIC X(10).
           03  QBW-BAD-ADR-FG                         PIC X(1).
           03  QBW-BAD-PHONE-FG                       PIC X(1).
           03  QBW-BAD-EMPPHONE-FG                    PIC X(1).
           03  QBW-BAD-MISC-PHONE1-FG                 PIC X(1).
           03  QBW-BAD-MISC-PHONE2-FG                 PIC X(1).
           03  QBW-BAD-CELL-FG                        PIC X(1).
           03  QBW-MISC-PHONE-1                       PIC 9(10).
           03  QBW-MISC-EXT-1                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-1                    PIC XX.
           03  QBW-MISC-PHONE-2                       PIC 9(10).
           03  QBW-MISC-EXT-2                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-2                    PIC XX.
           03  QBW-MISC-PHONE-3                       PIC 9(10).
           03  QBW-MISC-EXT-3                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-3                    PIC XX.
           03  QBW-MISC-PHONE-4                       PIC 9(10).
           03  QBW-MISC-EXT-4                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-4                    PIC XX.
           03  QBW-MISC-PHONE-5                       PIC 9(10).
           03  QBW-MISC-EXT-5                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-5                    PIC XX.
           03  QBW-MISC-PHONE-6                       PIC 9(10).
           03  QBW-MISC-EXT-6                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-6                    PIC XX.
           03  QBW-CONTACT-DESC-1                     PIC X(46).
           03  QBW-CONTACT-PHONE-1                    PIC 9(10).
           03  QBW-CONTACT-EXT-1                      PIC 9(4).
           03  QBW-CONTACT-RELATION-1                 PIC X(12).
           03  QBW-CONTACT-DESC-2                     PIC X(46).
           03  QBW-CONTACT-PHONE-2                    PIC 9(10).
           03  QBW-CONTACT-EXT-2                      PIC 9(4).
           03  QBW-CONTACT-RELATION-2                 PIC X(12).
           03  QBW-CONTACT-DESC-3                     PIC X(46).
           03  QBW-CONTACT-PHONE-3                    PIC 9(10).
           03  QBW-CONTACT-EXT-3                      PIC 9(4).
           03  QBW-CONTACT-RELATION-3                 PIC X(12).
           03  QBW-CONTACT-DESC-4                     PIC X(46).
           03  QBW-CONTACT-PHONE-4                    PIC 9(10).
           03  QBW-CONTACT-EXT-4                      PIC 9(4).
           03  QBW-CONTACT-RELATION-4                 PIC X(12).
           03  QBW-CONTACT-DESC-5                     PIC X(46).
           03  QBW-CONTACT-PHONE-5                    PIC 9(10).
           03  QBW-CONTACT-EXT-5                      PIC 9(4).
           03  QBW-CONTACT-RELATION-5                 PIC X(12).
           03  QBW-CONTACT-DESC-6                     PIC X(46).
           03  QBW-CONTACT-PHONE-6                    PIC 9(10).
           03  QBW-CONTACT-EXT-6                      PIC 9(4).
           03  QBW-CONTACT-RELATION-6                 PIC X(12).
           03  QBW-CONTACT-DESC-7                     PIC X(46).
           03  QBW-CONTACT-PHONE-7                    PIC 9(10).
           03  QBW-CONTACT-EXT-7                      PIC 9(4).
           03  QBW-CONTACT-RELATION-7                 PIC X(12).
           03  QBW-CONTACT-DESC-8                     PIC X(46).
           03  QBW-CONTACT-PHONE-8                    PIC 9(10).
           03  QBW-CONTACT-EXT-8                      PIC 9(4).
           03  QBW-CONTACT-RELATION-8                 PIC X(12).
           03  QBW-CONTACT-DESC-9                     PIC X(46).
           03  QBW-CONTACT-PHONE-9                    PIC 9(10).
           03  QBW-CONTACT-EXT-9                      PIC 9(4).
           03  QBW-CONTACT-RELATION-9                 PIC X(12).
           03  QBW-CONTACT-DESC-10                    PIC X(46).
           03  QBW-CONTACT-PHONE-10                   PIC 9(10).
           03  QBW-CONTACT-EXT-10                     PIC 9(4).
           03  QBW-CONTACT-RELATION-10                PIC X(12).
           03  QBW-SRCD                               PIC XX.
           03  QBW-CALL-ON-SUNDAY                     PIC X.
           03  QBW-SOLICIT                            PIC X.
           03  QBW-CONFIDENTIAL                       PIC X.
           03  QBW-PRIVACY-OPT-OUT                    PIC X.
           03  QBW-OPT-IN-EMAIL                       PIC X.
           03  QBW-OPT-IN-TEXT                        PIC X.
           03  QBW-CITY-TAX-CD                        PIC X(3).
           03  QBW-DEALTRACK-CHG-DATE                 PIC X(10).
           03  QBW-DECEASED                           PIC X.
           03  QBW-DECEASED-DATE                      PIC X(10).
           03  QBW-EMAIL                              PIC X(40).
           03  QBW-IDENT-NO                           PIC X(18).
           03  QBW-IDENT-TYPE                         PIC XX.
           03  QBW-IDENT-STATE                        PIC XX.
           03  QBW-MILITARY-FLAG                      PIC X.
           03  QBW-ORGBR                              PIC 9(4).

       01  QLN-REC.
           03  QLN-OWNBR                             PIC 9(4).
           03  QLN-ACCTNO                            PIC 9(8).
           03  QLN-CLASS                             PIC 9(3).
           03  QLN-SUBCLASS                          PIC 99.
           03  QLN-LAWCODE                           PIC 99.
           03  QLN-SSNO1                             PIC 9(9).
           03  QLN-MAKERCD1                          PIC X.
           03  QLN-SSNO2                             PIC 9(9).
           03  QLN-MAKERCD2                          PIC X.
           03  QLN-SSNO3                             PIC 9(9).
           03  QLN-MAKERCD3                          PIC X.
           03  QLN-SSNO4                             PIC 9(9).
           03  QLN-MAKERCD4                          PIC X.
           03  QLN-ORGST                             PIC XX.
           03  QLN-ORGBR                             PIC 9(4).
           03  QLN-OWNST                             PIC XX.
           03  QLN-SPRCLASS                          PIC 9(3).
           03  QLN-CONVERCD                          PIC X.
           03  QLN-MIDSTREAM                         PIC X.
           03  QLN-PURCD                             PIC XXX.
           03  QLN-SRCD                              PIC XX.
           03  QLN-COLLCD                            PIC X(2).
           03  QLN-COLLID                            PIC X(20).
           03  QLN-POCD                              PIC XX.
           03  QLN-PLCD                              PIC X.
           03  QLN-PLTYPE                            PIC X.
           03  QLN-CONFIDENTCD                       PIC X.
           03  QLN-ALLOTCD                           PIC X.
           03  QLN-RECASTCD                          PIC X.
           03  QLN-SKIP-TRW                          PIC X.
           03  QLN-STATUSFG                          PIC X.
           03  QLN-SMALLOAN                          PIC X.
           03  QLN-SALEFIN                           PIC X.
           03  QLN-SERVACCT                          PIC X.
           03  QLN-LOANTYPE                          PIC X.
           03  QLN-ADDON-FG                          PIC X.
           03  QLN-PAY-SCHLD-FG                      PIC X.
           03  QLN-LC-CODE                           PIC 99.
           03  QLN-CAN-LOAN-NO                       PIC XX.
           03  QLN-STATFLAGS                         PIC X(20).
           03  QLN-ENTDATE                           PIC X(10).
           03  QLN-LOANDATE                          PIC X(10).
           03  QLN-PURDATE                           PIC X(10).
           03  QLN-INTDATE                           PIC X(10).
           03  QLN-CASHDATE                          PIC X(10).
           03  QLN-FILEDATE                          PIC X(10).
           03  QLN-RESCISSION-DATE                   PIC X(10).
           03  QLN-PART-RECOURSE-DATE                PIC X(10).
           03  QLN-ORIG-1STPYDATE                    PIC X(10).
           03  QLN-1STPYDATE                         PIC X(10).
           03  QLN-JDDATE                            PIC X(10).
           03  QLN-PLDATE                            PIC X(10).
           03  QLN-PL-TRDATE                         PIC X(10).
           03  QLN-ACCELDATE                         PIC X(10).
           03  QLN-REDUDATE                          PIC X(10).
           03  QLN-POFFDATE                          PIC X(10).
           03  QLN-PO-TRDATE                         PIC X(10).
           03  QLN-LEGALDATE                         PIC X(10).
           03  QLN-BNKRPTDATE                        PIC X(10).
           03  QLN-BNKRPT-DISMISS-DATE               PIC X(10).
           03  QLN-BNKRPT-DISCHARGE-DATE             PIC X(10).
           03  QLN-BNKRPT-REAFFIRM-DATE              PIC X(10).
           03  QLN-DEBTAIDATE                        PIC X(10).
           03  QLN-REPODATE                          PIC X(10).
           03  QLN-REDEEMDATE                        PIC X(10).
           03  QLN-SOLDDATE                          PIC X(10).
           03  QLN-INS-CONFIRM-DATE                  PIC X(10).
           03  QLN-PRIOR-RN-FOR10-DATE               PIC X(10).
           03  QLN-LAST-RN-FOR10-DATE                PIC X(10).
           03  QLN-TRW-DOFD-DATE                     PIC X(10).
           03  QLN-LAST-PYPA-DATE                    PIC X(10).
           03  QLN-BT-TRDATE                         PIC X(10).
           03  QLN-METRO2-LAST-PMT-DATE              PIC X(10).
           03  QLN-FOLLOWUP-DATE                     PIC X(10).
           03  QLN-LAST-ACCRUAL-DATE                 PIC X(10).
           03  QLN-APRATE                            PIC 999V999.
           03  QLN-EFFRATE                           PIC 999V999.
           03  QLN-SMPRATE                           PIC 999V999.
           03  QLN-YLDRATE                           PIC 999V999.
           03  QLN-OVRDRATE                          PIC 999V999.
           03  QLN-ENDORATE                          PIC 999V999.
           03  QLN-JDRATE                            PIC 999V999.
           03  QLN-ACCRATE                           PIC 999V999.
           03  QLN-REDURATE                          PIC 999V999.
           03  QLN-CONRATE                           PIC 999V999.
           03  QLN-SERVPER                           PIC 9(3)V99.
           03  QLN-SELLPR                            PIC 9(7)V99.
           03  QLN-DOWNPY                            PIC 9(7)V99.
           03  QLN-TRADEIN                           PIC S9(7)V99.
           03  QLN-LNAMT                             PIC 9(7)V99.
           03  QLN-FINCHG                            PIC 9(7)V99.
           03  QLN-INTCHG                            PIC 9(7)V99.
           03  QLN-SERCHG                            PIC 9(5)V99.
           03  QLN-EXTCHG                            PIC 9(5)V99.
           03  QLN-MAINTFEE                          PIC 9(5)V99.
           03  QLN-POINTS                            PIC 99V99.
           03  QLN-PROCEEDS                          PIC S9(7)V99.
           03  QLN-ORGTERM                           PIC 9(3).
           03  QLN-1STPYAMT                          PIC 9(7)V99.
           03  QLN-NOREGPY                           PIC 9(3).
           03  QLN-REGPYAMT                          PIC 9(7)V99 .
           03  QLN-LASTPYAMT                         PIC S9(7)V99.
           03  QLN-PROCASH                           PIC S9(7)V99.
           03  QLN-TILLNO                            PIC 9(2).
           03  QLN-STATE-FLAG                        PIC X.
           03  QLN-STATE-WORKER                      PIC S9(7)V99.
           03  QLN-STATE-WORKER2                     PIC S9(5)V99.
           03  QLN-FEECODE-1                         PIC XX.
           03  QLN-FEEAMT-1                          PIC 9(5)V99.
           03  QLN-FEEOURS-1                         PIC X.
           03  QLN-FEEFINCHG-1                       PIC X.
           03  QLN-FEECODE-2                         PIC XX.
           03  QLN-FEEAMT-2                          PIC 9(5)V99.
           03  QLN-FEEOURS-2                         PIC X.
           03  QLN-FEEFINCHG-2                       PIC X.
           03  QLN-FEECODE-3                         PIC XX.
           03  QLN-FEEAMT-3                          PIC 9(5)V99.
           03  QLN-FEEOURS-3                         PIC X.
           03  QLN-FEEFINCHG-3                       PIC X.
           03  QLN-FEECODE-4                         PIC XX.
           03  QLN-FEEAMT-4                          PIC 9(5)V99.
           03  QLN-FEEOURS-4                         PIC X.
           03  QLN-FEEFINCHG-4                       PIC X.
           03  QLN-FEECODE-5                         PIC XX.
           03  QLN-FEEAMT-5                          PIC 9(5)V99.
           03  QLN-FEEOURS-5                         PIC X.
           03  QLN-FEEFINCHG-5                       PIC X.
           03  QLN-FEECODE-6                         PIC XX.
           03  QLN-FEEAMT-6                          PIC 9(5)V99.
           03  QLN-FEEOURS-6                         PIC X.
           03  QLN-FEEFINCHG-6                       PIC X.
           03  QLN-FEECODE-7                         PIC XX.
           03  QLN-FEEAMT-7                          PIC 9(5)V99.
           03  QLN-FEEOURS-7                         PIC X.
           03  QLN-FEEFINCHG-7                       PIC X.
           03  QLN-FEECODE-8                         PIC XX.
           03  QLN-FEEAMT-8                          PIC 9(5)V99.
           03  QLN-FEEOURS-8                         PIC X.
           03  QLN-FEEFINCHG-8                       PIC X.
           03  QLN-FEECODE-9                         PIC XX.
           03  QLN-FEEAMT-9                          PIC 9(5)V99.
           03  QLN-FEEOURS-9                         PIC X.
           03  QLN-FEEFINCHG-9                       PIC X.
           03  QLN-FEECODE-10                        PIC XX.
           03  QLN-FEEAMT-10                         PIC 9(5)V99.
           03  QLN-FEEOURS-10                        PIC X.
           03  QLN-FEEFINCHG-10                      PIC X.
           03  QLN-FEECODE-11                        PIC XX.
           03  QLN-FEEAMT-11                         PIC 9(5)V99.
           03  QLN-FEEOURS-11                        PIC X.
           03  QLN-FEEFINCHG-11                      PIC X.
           03  QLN-FEECODE-12                        PIC XX.
           03  QLN-FEEAMT-12                         PIC 9(5)V99.
           03  QLN-FEEOURS-12                        PIC X.
           03  QLN-FEEFINCHG-12                      PIC X.
           03  QLN-FEECODE-13                        PIC XX.
           03  QLN-FEEAMT-13                         PIC 9(5)V99.
           03  QLN-FEEOURS-13                        PIC X.
           03  QLN-FEEFINCHG-13                      PIC X.
           03  QLN-FEECODE-14                        PIC XX.
           03  QLN-FEEAMT-14                         PIC 9(5)V99.
           03  QLN-FEEOURS-14                        PIC X.
           03  QLN-FEEFINCHG-14                      PIC X.
           03  QLN-FEECODE-15                        PIC XX.
           03  QLN-FEEAMT-15                         PIC 9(5)V99.
           03  QLN-FEEOURS-15                        PIC X.
           03  QLN-FEEFINCHG-15                      PIC X.
           03  QLN-FEECODE-16                        PIC XX.
           03  QLN-FEEAMT-16                         PIC 9(5)V99.
           03  QLN-FEEOURS-16                        PIC X.
           03  QLN-FEEFINCHG-16                      PIC X.
           03  QLN-FEECODE-17                        PIC XX.
           03  QLN-FEEAMT-17                         PIC 9(5)V99.
           03  QLN-FEEOURS-17                        PIC X.
           03  QLN-FEEFINCHG-17                      PIC X.
           03  QLN-FEECODE-18                        PIC XX.
           03  QLN-FEEAMT-18                         PIC 9(5)V99.
           03  QLN-FEEOURS-18                        PIC X.
           03  QLN-FEEFINCHG-18                      PIC X.
           03  QLN-FEECODE-19                        PIC XX.
           03  QLN-FEEAMT-19                         PIC 9(5)V99.
           03  QLN-FEEOURS-19                        PIC X.
           03  QLN-FEEFINCHG-19                      PIC X.
           03  QLN-FEECODE-20                        PIC XX.
           03  QLN-FEEAMT-20                         PIC 9(5)V99.
           03  QLN-FEEOURS-20                        PIC X.
           03  QLN-FEEFINCHG-20                      PIC X.
           03  QLN-INSURED                           PIC X.
           03  QLN-INSTYPES                          PIC X.
           03  QLN-INSELIGIBLE                       PIC X.
           03  QLN-INSCOMP-1                         PIC 99.
           03  QLN-INSOURS-1                         PIC X.
           03  QLN-INSEFF-DATE-1                     PIC X(10).
           03  QLN-INSEXP-DATE-1                     PIC X(10).
           03  QLN-INSPREM-1                         PIC 9(5)V99.
           03  QLN-INSCOVR-1                         PIC 9(7)V99.
           03  QLN-INSCOMM-1                         PIC 9(5)V99.
           03  QLN-INSANTICERN-1                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-1                     PIC S9(7)V99.
           03  QLN-INSUFERN-1                        PIC S9(7)V99.
           03  QLN-INSACCERN-1                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-1                   PIC 99.
           03  QLN-INSFINCHG-1                       PIC X.
           03  QLN-INSCOMP-2                         PIC 99.
           03  QLN-INSOURS-2                         PIC X.
           03  QLN-INSEFF-DATE-2                     PIC X(10).
           03  QLN-INSEXP-DATE-2                     PIC X(10).
           03  QLN-INSPREM-2                         PIC 9(5)V99.
           03  QLN-INSCOVR-2                         PIC 9(7)V99.
           03  QLN-INSCOMM-2                         PIC 9(5)V99.
           03  QLN-INSANTICERN-2                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-2                     PIC S9(7)V99.
           03  QLN-INSUFERN-2                        PIC S9(7)V99.
           03  QLN-INSACCERN-2                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-2                   PIC 99.
           03  QLN-INSFINCHG-2                       PIC X.
           03  QLN-INSCOMP-3                         PIC 99.
           03  QLN-INSOURS-3                         PIC X.
           03  QLN-INSEFF-DATE-3                     PIC X(10).
           03  QLN-INSEXP-DATE-3                     PIC X(10).
           03  QLN-INSPREM-3                         PIC 9(5)V99.
           03  QLN-INSCOVR-3                         PIC 9(7)V99.
           03  QLN-INSCOMM-3                         PIC 9(5)V99.
           03  QLN-INSANTICERN-3                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-3                     PIC S9(7)V99.
           03  QLN-INSUFERN-3                        PIC S9(7)V99.
           03  QLN-INSACCERN-3                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-3                   PIC 99.
           03  QLN-INSFINCHG-3                       PIC X.
           03  QLN-INSCOMP-4                         PIC 99.
           03  QLN-INSOURS-4                         PIC X.
           03  QLN-INSEFF-DATE-4                     PIC X(10).
           03  QLN-INSEXP-DATE-4                     PIC X(10).
           03  QLN-INSPREM-4                         PIC 9(5)V99.
           03  QLN-INSCOVR-4                         PIC 9(7)V99.
           03  QLN-INSCOMM-4                         PIC 9(5)V99.
           03  QLN-INSANTICERN-4                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-4                     PIC S9(7)V99.
           03  QLN-INSUFERN-4                        PIC S9(7)V99.
           03  QLN-INSACCERN-4                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-4                   PIC 99.
           03  QLN-INSFINCHG-4                       PIC X.
           03  QLN-INSCOMP-5                         PIC 99.
           03  QLN-INSOURS-5                         PIC X.
           03  QLN-INSEFF-DATE-5                     PIC X(10).
           03  QLN-INSEXP-DATE-5                     PIC X(10).
           03  QLN-INSPREM-5                         PIC 9(5)V99.
           03  QLN-INSCOVR-5                         PIC 9(7)V99.
           03  QLN-INSCOMM-5                         PIC 9(5)V99.
           03  QLN-INSANTICERN-5                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-5                     PIC S9(7)V99.
           03  QLN-INSUFERN-5                        PIC S9(7)V99.
           03  QLN-INSACCERN-5                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-5                   PIC 99.
           03  QLN-INSFINCHG-5                       PIC X.
           03  QLN-INSCOMP-6                         PIC 99.
           03  QLN-INSOURS-6                         PIC X.
           03  QLN-INSEFF-DATE-6                     PIC X(10).
           03  QLN-INSEXP-DATE-6                     PIC X(10).
           03  QLN-INSPREM-6                         PIC 9(5)V99.
           03  QLN-INSCOVR-6                         PIC 9(7)V99.
           03  QLN-INSCOMM-6                         PIC 9(5)V99.
           03  QLN-INSANTICERN-6                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-6                     PIC S9(7)V99.
           03  QLN-INSUFERN-6                        PIC S9(7)V99.
           03  QLN-INSACCERN-6                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-6                   PIC 99.
           03  QLN-INSFINCHG-6                       PIC X.
           03  QLN-INSCOMP-7                         PIC 99.
           03  QLN-INSOURS-7                         PIC X.
           03  QLN-INSEFF-DATE-7                     PIC X(10).
           03  QLN-INSEXP-DATE-7                     PIC X(10).
           03  QLN-INSPREM-7                         PIC 9(5)V99.
           03  QLN-INSCOVR-7                         PIC 9(7)V99.
           03  QLN-INSCOMM-7                         PIC 9(5)V99.
           03  QLN-INSANTICERN-7                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-7                     PIC S9(7)V99.
           03  QLN-INSUFERN-7                        PIC S9(7)V99.
           03  QLN-INSACCERN-7                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-7                   PIC 99.
           03  QLN-INSFINCHG-7                       PIC X.
           03  QLN-INSCOMP-8                         PIC 99.
           03  QLN-INSOURS-8                         PIC X.
           03  QLN-INSEFF-DATE-8                     PIC X(10).
           03  QLN-INSEXP-DATE-8                     PIC X(10).
           03  QLN-INSPREM-8                         PIC 9(5)V99.
           03  QLN-INSCOVR-8                         PIC 9(7)V99.
           03  QLN-INSCOMM-8                         PIC 9(5)V99.
           03  QLN-INSANTICERN-8                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-8                     PIC S9(7)V99.
           03  QLN-INSUFERN-8                        PIC S9(7)V99.
           03  QLN-INSACCERN-8                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-8                   PIC 99.
           03  QLN-INSFINCHG-8                       PIC X.
           03  QLN-INSCOMP-9                         PIC 99.
           03  QLN-INSOURS-9                         PIC X.
           03  QLN-INSEFF-DATE-9                     PIC X(10).
           03  QLN-INSEXP-DATE-9                     PIC X(10).
           03  QLN-INSPREM-9                         PIC 9(5)V99.
           03  QLN-INSCOVR-9                         PIC 9(7)V99.
           03  QLN-INSCOMM-9                         PIC 9(5)V99.
           03  QLN-INSANTICERN-9                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-9                     PIC S9(7)V99.
           03  QLN-INSUFERN-9                        PIC S9(7)V99.
           03  QLN-INSACCERN-9                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-9                   PIC 99.
           03  QLN-INSFINCHG-9                       PIC X.
           03  QLN-INSCOMP-10                        PIC 99.
           03  QLN-INSOURS-10                        PIC X.
           03  QLN-INSEFF-DATE-10                    PIC X(10).
           03  QLN-INSEXP-DATE-10                    PIC X(10).
           03  QLN-INSPREM-10                        PIC 9(5)V99.
           03  QLN-INSCOVR-10                        PIC 9(7)V99.
           03  QLN-INSCOMM-10                        PIC 9(5)V99.
           03  QLN-INSANTICERN-10                    PIC S9(7)V99.
           03  QLN-INSANTICADJ-10                    PIC S9(7)V99.
           03  QLN-INSUFERN-10                       PIC S9(7)V99.
           03  QLN-INSACCERN-10                      PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-10                  PIC 99.
           03  QLN-INSFINCHG-10                      PIC X.
           03  QLN-PY-YYYYMM-RECDEL                  PIC 9(6).
           03  QLN-MTD-PAYS-RECDEL                   PIC S9(7)V99.
           03  QLN-GARNISH-EXTR                      PIC X.
           03  QLN-SHIP-SAME-FG                      PIC X.
           03  QLN-AD-D-INS-COVR                     PIC 9(7).
           03  QLN-TOT-PARTIAL-CHGOFF                PIC S9(7)V99.
           03  QLN-LPO                               PIC X(4).
           03  QLN-REP-CD                            PIC X(8).
           03  QLN-EMP-ID                            PIC 9(8).
           03  QLN-PROCESS-CD                        PIC X.
           03  QLN-WCBC-CREDITLIM                    PIC S9(5).
           03  QLN-FOLLOWUP-CD                       PIC X(4).
           03  QLN-MIP-FG                            PIC X.
           03  QLN-MIP-MTD-CLAHPAID                  PIC S9(5)V99.
           03  QLN-MIP-CLPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-CLPAID                    PIC S9(7)V99.
           03  QLN-MIP-AHPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-AHPAID                    PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME                   PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME2                  PIC S9(7)V99.
           03  QLN-RESCISSION                        PIC X(1).
           03  QLN-INTERIM-TRW                       PIC X(1).
           03  QLN-DLNO                              PIC 9(4)V99.
           03  QLN-DLPOOL                            PIC 9(5).
           03  QLN-DLRECOURSE                        PIC X.
           03  QLN-DLPARTRCAMT                       PIC 9(7)V99.
           03  QLN-ORIGPURAMT                        PIC 9(7)V99.
           03  QLN-PURBAL                            PIC S9(7)V99.
           03  QLN-DLRVPRIN-1                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-2                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-3                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-1                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-2                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-3                        PIC 9(7)V99.
           03  QLN-DLPARVHELD                        PIC 9(7)V99.
           03  QLN-DLPARVPAID                        PIC 9(7)V99.
           03  QLN-DLDISC                            PIC 9(7)V99.
           03  QLN-LCPDTH-DATE                       PIC X(10).
           03  QLN-INTPDTH-DATE                      PIC X(10).
           03  QLN-CURBAL                            PIC S9(7)V99.
           03  QLN-INTBAL                            PIC S9(7)V99.
           03  QLN-LCBAL                             PIC S9(7)V99.
           03  QLN-OTHBAL                            PIC S9(7)V99.
           03  QLN-OT2BAL                            PIC S9(7)V99.
           03  QLN-LCPAID                            PIC S9(7)V99.
           03  QLN-LCPARTIALS                        PIC S9(7)V99.
           03  QLN-STMT-COUNTER                      PIC 9(5).
           03  QLN-PRIOR-STMT-COUNTER                PIC 9(5).
           03  QLN-FROZEN-REASON                     PIC X(2).
           03  QLN-COUPON-STATEMENT                  PIC X.
           03  QLN-MONTHEND-BAL                      PIC 9(7)V99.
           03  QLN-ANTICTERM                         PIC 9(3).
           03  QLN-ANTICERN-1                        PIC S9(7)V99.
           03  QLN-ANTICADJ-1                        PIC S9(7)V99.
           03  QLN-UFERN-1                           PIC S9(7)V99.
           03  QLN-ACCERN-1                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-1                      PIC 99.
           03  QLN-ANTICERN-2                        PIC S9(7)V99.
           03  QLN-ANTICADJ-2                        PIC S9(7)V99.
           03  QLN-UFERN-2                           PIC S9(7)V99.
           03  QLN-ACCERN-2                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-2                      PIC 99.
           03  QLN-ANTICERN-3                        PIC S9(7)V99.
           03  QLN-ANTICADJ-3                        PIC S9(7)V99.
           03  QLN-UFERN-3                           PIC S9(7)V99.
           03  QLN-ACCERN-3                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-3                      PIC 99.
           03  QLN-ANTICERN-4                        PIC S9(7)V99.
           03  QLN-ANTICADJ-4                        PIC S9(7)V99.
           03  QLN-UFERN-4                           PIC S9(7)V99.
           03  QLN-ACCERN-4                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-4                      PIC 99.
           03  QLN-ANTICERN-5                        PIC S9(7)V99.
           03  QLN-ANTICADJ-5                        PIC S9(7)V99.
           03  QLN-UFERN-5                           PIC S9(7)V99.
           03  QLN-ACCERN-5                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-5                      PIC 99.
           03  QLN-ANTICERN-6                        PIC S9(7)V99.
           03  QLN-ANTICADJ-6                        PIC S9(7)V99.
           03  QLN-UFERN-6                           PIC S9(7)V99.
           03  QLN-ACCERN-6                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-6                      PIC 99.
           03  QLN-ANTICERN-7                        PIC S9(7)V99.
           03  QLN-ANTICADJ-7                        PIC S9(7)V99.
           03  QLN-UFERN-7                           PIC S9(7)V99.
           03  QLN-ACCERN-7                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-7                      PIC 99.
           03  QLN-ANTICERN-8                        PIC S9(7)V99.
           03  QLN-ANTICADJ-8                        PIC S9(7)V99.
           03  QLN-UFERN-8                           PIC S9(7)V99.
           03  QLN-ACCERN-8                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-8                      PIC 99.
           03  QLN-ANTICERN-9                        PIC S9(7)V99.
           03  QLN-ANTICADJ-9                        PIC S9(7)V99.
           03  QLN-UFERN-9                           PIC S9(7)V99.
           03  QLN-ACCERN-9                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-9                      PIC 99.
           03  QLN-ANTICERN-10                       PIC S9(7)V99.
           03  QLN-ANTICADJ-10                       PIC S9(7)V99.
           03  QLN-UFERN-10                          PIC S9(7)V99.
           03  QLN-ACCERN-10                         PIC S9(7)V99.
           03  QLN-EARN-ORDER-10                     PIC 99.
           03  QLN-LMO-PRIOR-INTEREST                PIC S9(7)V99.
           03  QLN-PRIOR-UNEARNED                    PIC S9(7)V99.
           03  QLN-ERNDELQ-STOP                      PIC X.
           03  QLN-NONACRERN                         PIC S9(7)V99.
           03  QLN-PRIOR-NONACRERN                   PIC S9(7)V99.
           03  QLN-ERN-IB-PDTH-DATE                  PIC X(10).
           03  QLN-ERN-IB-PRINBAL                    PIC S9(7)V99.
           03  QLN-ERN-IB-INTOWE                     PIC S9(7)V99.
           03  QLN-TOTERN                            PIC S9(7)V99.
           03  QLN-TOTOTH                            PIC S9(7)V99.
           03  QLN-TOTINT                            PIC S9(7)V99.
           03  QLN-TOTLCHG                           PIC S9(7)V99.
           03  QLN-TOTDEF                            PIC S9(7)V99.
           03  QLN-TOTSERACCT                        PIC S9(7)V99.
           03  QLN-LCWAIVERS                         PIC S9(5)V99.
           03  QLN-INTWAIVERS                        PIC S9(5)V99.
           03  QLN-TOTPAYMNTD                        PIC S9(7)V99.
           03  QLN-TOTEXCPAYMNTD                     PIC S9(7)V99.
           03  QLN-TOTNODEF                          PIC S99.
           03  QLN-YTDNODEF                          PIC S99.
           03  QLN-TOTNODEFACT                       PIC S99.
           03  QLN-YTDNODEFACT                       PIC S99.
           03  QLN-TOTNOALDEL                        PIC S99.
           03  QLN-YTDNOALDEL                        PIC S99.
           03  QLN-YTDINTPD                          PIC S9(7)V99.
           03  QLN-LYRINTPD                          PIC S9(7)V99.
           03  QLN-TOTINTPD                          PIC S9(7)V99.
           03  QLN-RECASTINT                         PIC 9(7)V99.
           03  QLN-RECASTPRIN                        PIC 9(7)V99.
           03  QLN-CL-REBATE                         PIC X.
           03  QLN-AH-REBATE                         PIC X.
           03  QLN-PP-REBATE                         PIC X.
           03  QLN-INT-REBATE                        PIC X.
           03  QLN-SC-REBATE                         PIC X.
           03  QLN-MF-REBATE                         PIC X.
           03  QLN-DF-REBATE                         PIC X.
           03  QLN-EX-REBATE                         PIC X.
           03  QLN-O1-REBATE                         PIC X.
           03  QLN-O2-REBATE                         PIC X.
           03  QLN-O3-REBATE                         PIC X.
           03  QLN-O4-REBATE                         PIC X.
           03  QLN-O5-REBATE                         PIC X.
           03  QLN-O6-REBATE                         PIC X.
           03  QLN-O7-REBATE                         PIC X.
           03  QLN-UN-O7-REB                         PIC X.
           03  QLN-UN-O8-REB                         PIC X.
           03  QLN-UN-O9-REB                         PIC X.
           03  QLN-UN-O10-REB                        PIC X.
           03  QLN-PLREASON                          PIC XX.
           03  QLN-COLLECTOR                         PIC X(3).
           03  QLN-ACTIONCD                          PIC X(4).
           03  QLN-ACTIONCD2                         PIC X(4).
           03  QLN-ATCODE                            PIC X(3).
           03  QLN-RECODE                            PIC X(3).
           03  QLN-ESCROW-FG                         PIC X.
           03  QLN-PROMDAYS-PASTDUE                  PIC S9(3).
           03  QLN-PROMID                            PIC X(3).
           03  QLN-PROMDATE                          PIC X(10).
           03  QLN-PROMAMT                           PIC 9(7)V99.
           03  QLN-PROMDATE2                         PIC X(10).
           03  QLN-DATE-PAID-LAST                    PIC X(10).
           03  QLN-AMT-PAID-LAST                     PIC 9(7)V99.
           03  QLN-LASTCFORMNO                       PIC 9(5).
           03  QLN-LASTCFORMDATE                     PIC X(10).
           03  QLN-DELCNT                            PIC 99.
           03  QLN-DELCNT2                           PIC 99.
           03  QLN-CREDITLIM                         PIC 9(7).
           03  QLN-CRSCORE                           PIC S9(3).
           03  QLN-LASTSFORMNO                       PIC 9(5).
           03  QLN-LASTSFORMDATE                     PIC X(10).
           03  QLN-SOLCNT                            PIC 99.
           03  QLN-MESSAGE                           PIC X(30).
           03  QLN-OLDACCTNO                         PIC 9(8).
           03  QLN-PRLNNO                            PIC 9(8).
           03  QLN-PRBAL                             PIC S9(7)V99.
           03  QLN-PRINTDUE                          PIC S9(7)V99.
           03  QLN-PRFINCHGRB                        PIC S9(7)V99.
           03  QLN-PRINSRB                           PIC S9(7)V99.
           03  QLN-PRPOCD                            PIC XX.
           03  QLN-PRPODATE                          PIC X(10).
           03  QLN-PRDELINQAMT                       PIC S9(7)V99.
           03  QLN-PRLNAMT                           PIC 9(7)V99.
           03  QLN-PRPDUE-DAYS                       PIC S9(5).
           03  QLN-PRDLDISC-UNEARN                   PIC S9(7)V99.
           03  QLN-PRCONPROFILE                      PIC 9(12).
           03  QLN-SUM-ORIGPURAMT                    PIC 9(7)V99.
           03  QLN-SUM-LNAMT                         PIC 9(7)V99.
           03  QLN-SUM-FINCHG                        PIC 9(7)V99.
           03  QLN-SUM-INTCHG                        PIC 9(7)V99.
           03  QLN-SUM-ORGTERM                       PIC 9(3).
           03  QLN-SUM-REGPYAMT                      PIC 9(7)V99.
           03  QLN-SUM-EXTCHG                        PIC 9(5)V99.
           03  QLN-SUM-SERCHG                        PIC 9(5)V99.
           03  QLN-SUM-MAINTFEE                      PIC 9(7)V99.
           03  QLN-UNITPER-CD                        PIC X.
           03  QLN-UNITPER-FREQ                      PIC 999.
           03  QLN-DAYS-IN-UNITPER                   PIC 9(3).
           03  QLN-UNITPER-PER-YEAR                  PIC 9(3)V9(12).
           03  QLN-REPOCD                            PIC X.
           03  QLN-BR-TFR-FG                         PIC X.
           03  QLN-O3INS-DLPROC                      PIC 9(7)V99.
           03  QLN-INSPREM-DLADJ                     PIC S9(5)V99.
           03  QLN-PLAMT                             PIC 9(7)V99.
           03  QLN-MTD-PRIN                          PIC S9(7)V99.
           03  QLN-MTD-LC                            PIC S9(5)V99.
           03  QLN-MTD-INT                           PIC S9(5)V99.
           03  QLN-MTD-OT                            PIC S9(5)V99.
           03  QLN-MTD-EX                            PIC S9(7)V99.
           03  QLN-BNKRPT-CHAPTER                    PIC 9(2).
           03  QLN-BMO-CON-STATUS                    PIC X.
           03  QLN-BMO-REC-STATUS                    PIC X.
           03  QLN-APP-NUMBER                        PIC 9(09).
           03  QLN-GIFTNET                           PIC X.
           03  QLN-GIFTNET-OUR-PO                    PIC 9(7).
           03  QLN-PROC-DISBURSED                    PIC X.
           03  QLN-NOODDPY                           PIC X.
           03  QLN-CONPROFILE-1                      PIC 9(12).
           03  QLN-CONPROFILE-2                      PIC 9(12).
           03  QLN-CONPROFILE-3                      PIC 9(12).
           03  QLN-CONPROFILE-4                      PIC 9(12).
           03  QLN-CONPROFILE-5                      PIC 9(12).
           03  QLN-RECPROFILE-1                      PIC 9(12).
           03  QLN-RECPROFILE-2                      PIC 9(12).
           03  QLN-RECPROFILE-3                      PIC 9(12).
           03  QLN-RECPROFILE-4                      PIC 9(12).
           03  QLN-RECPROFILE-5                      PIC 9(12).
           03  QLN-TRW-STATUS                        PIC XX.
           03  QLN-ORIGIN                            PIC X(4).
           03  QLN-PO-TRCD                           PIC XX.
           03  QLN-NEXIS-CRSCORE                     PIC 9(3).
           03  QLN-TRW-B-CONSUMER-IND                PIC X(2).
           03  QLN-PARVHELD-PAID-FG                  PIC X.
           03  QLN-ACCT-FROZEN                       PIC X.
           03  QLN-TRW-SPECIAL-CODE                  PIC X(2).
           03  QLN-CREATION-STATUS                   PIC X(1).
           03  QLN-TOTNOLCHG                         PIC S9(3).
           03  QLN-LBOX                              PIC X(1).
           03  QLN-EXTRA-PRIN-LFP                    PIC S9(7)V99.
           03  QLN-REMAIN-INT-SINCE-LFP              PIC S9(7)V99.
           03  QLN-REMAIN-PRIN-SINCE-LFP             PIC S9(7)V99.
           03  QLN-INTERIM-COUPON                    PIC X.
           03  QLN-DLCOMM-O3-CMSI                    PIC 9(5)V99.
           03  QLN-DLCOMM-PP-CMSI                    PIC 9(5)V99.
           03  QLN-TRW-J-CONSUMER-IND                PIC X(2).
           03  QLN-ACH                               PIC X.
           03  QLN-1099-C-FG                         PIC X.
           03  QLN-TRW-COMPLIANCE-CODE               PIC XX.
           03  QLN-UNWIND-REASON-CODE                PIC X(2).
           03  QLN-PBI-PBD-FG                        PIC X.
           03  QLN-OLD-ACCT-FORMAT-FG                PIC X.
           03  QLN-APPROVED-BY-USER                  PIC X(10).
           03  QLN-APPROVED-BY-CODE                  PIC X(3).
           03  QLN-APPROVED-BY-DATE                  PIC X(10).
           03  QLN-APPROVED-BY-TIME                  PIC 9(6).
           03  QLN-LTOUCH-DATE                       PIC X(10).
           03  QLN-LTOUCH-TIME                       PIC 9(6).

       01  QLP-REC.
           03  QLP-BRNO             PIC  9(4).
           03  QLP-ACCTNO           PIC  9(8).
           03  QLP-SEQNO            PIC  9(5).
           03  QLP-LTOUCH-DATE          PIC X(10).
           03  QLP-USERID               PIC X(10).
           03  QLP-POSTDATE             PIC X(10).
           03  QLP-POSTTIME             PIC 9(6).
           03  QLP-TRDATE               PIC X(10).
           03  QLP-PAYDATE              PIC X(10).
           03  QLP-TRCD                 PIC XX.
           03  QLP-REFNO                PIC X(5).
           03  QLP-TRAMT                PIC S9(7)V99.
           03  QLP-APOTH                PIC S9(7)V99.
           03  QLP-APOT2                PIC S9(7)V99.
           03  QLP-APINT                PIC S9(7)V99.
           03  QLP-APLC                 PIC S9(7)V99.
           03  QLP-APCUR                PIC S9(7)V99.
           03  QLP-OTHBAL               PIC S9(7)V99.
           03  QLP-OT2BAL               PIC S9(7)V99.
           03  QLP-INTBAL               PIC S9(7)V99.
           03  QLP-LCBAL                PIC S9(7)V99.
           03  QLP-CURBAL               PIC S9(7)V99.
           03  QLP-INTDUE               PIC S9(7)V99.
           03  QLP-DLPROC               PIC S9(7)V99.
           03  QLP-EARNED-1             PIC S9(7)V99.
           03  QLP-EARNED-2             PIC S9(7)V99.
           03  QLP-EARNED-3             PIC S9(7)V99.
           03  QLP-INTPAID              PIC S9(7)V99.
           03  QLP-SSNO                 PIC 9(10).
           03  QLP-PDTH-DATE            PIC X(10).
           03  QLP-LCPDTH-DATE          PIC X(10).
           03  QLP-INTPDTH-DATE         PIC X(10).
           03  QLP-TILLNO               PIC 99.
           03  QLP-APINTOWE             PIC S9(7)V99.
           03  QLP-DLPARVHELD-ADJ       PIC S9(7)V99.
           03  QLP-LT-SEQNO             PIC 9(3).
           03  QLP-IBPC                 PIC X.
           03  QLP-MANUAL-RB            PIC X.
           03  QLP-INSTYPES             PIC X.
           03  QLP-REV                  PIC X.
           03  QLP-REVTRCD              PIC XX.
           03  QLP-REVSEQNO             PIC 9(5).
           03  QLP-STATUSFG             PIC X.
           03  QLP-NEWSTAT-RATE         PIC 999V999.
           03  QLP-LCPARTIALS           PIC S9(7)V99.
           03  QLP-ADDON-ORIG-REGPY     PIC S9(7)V99.
           03  QLP-NEW1ST-PYDATE        PIC X(10).
           03  QLP-DEF-DUEDATE          PIC X(10).
           03  QLP-LCPAID               PIC S9(7)V99.
           03  QLP-OLD1ST-PYDATE        PIC X(10).
           03  QLP-DEF-TERM             PIC 9(3).
           03  QLP-ADDON-LAST-PYAMT     PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-1       PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-2       PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-3       PIC S9(7)V99.
           03  QLP-DLPARVPAID-ADJ       PIC S9(7)V99.
           03  QLP-DLNO                  PIC 9(4)V99.
           03  QLP-PREPAY-PENALTY        PIC S9(7)V99.
           03  QLP-PRORATED-INS-REB      PIC S9(7)V99.
           03  QLP-POSTING-BR            PIC 9(4).
           03  QLP-NOLCHG                PIC S9(3).
           03  QLP-COLLECTOR             PIC X(3).
           03  QLP-EXTRA-PRIN-LFP        PIC S9(7)V99.
           03  QLP-REMAIN-INT-SINCE-LFP  PIC S9(7)V99.
           03  QLP-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99.
           03  QLP-APPLIED-TO-ESCROW     PIC S9(7)V99.
           03  QLP-ESCROW-PREPAID        PIC S9(7)V99.
           03  QLP-PRIOR-POOLID          PIC 9(5).
           03  QLP-ADDON-INTPDTH-DATE    PIC X(10).
           03  QLP-FROMCL                PIC  999.
           03  QLP-TOCL                  PIC  999.
           03  QLP-ALT-PREPAY-FG         PIC X.

       01  QSP-REC.
           03  QSPA-REC.
               05  QSPA-ORGST                         PIC XX.
               05  QSPA-SPRCLASS                      PIC 999.
               05  QSPA-SUBCLASS                      PIC 99.
               05  QSPA-LAWCODE                       PIC 99.
               05  QSPA-LTOUCH-DATE                   PIC X(10).
               05  QSPA-DESC                          PIC X(15).
               05  QSPA-NOTES                         PIC X(60).
               05  QSPA-EFFDATE                       PIC X(10).
               05  QSPA-EXPDATE                       PIC X(10).
               05  QSPA-EXPD-MONTHS                   PIC 99.
               05  QSPA-EXPD-DAYS                     PIC 99.
               05  QSPA-EXPD-FRMLA                    PIC X.
               05  QSPA-EXPD-EXCEPT                   PIC X.
               05  QSPA-EXPR-FRMLA                    PIC X.
               05  QSPA-EXPR-REDU-FRMLA               PIC X.
               05  QSPA-EXPR-RATE-1                   PIC 999V999.
               05  QSPA-EXPR-BREAK-1                  PIC 9(7)V99.
               05  QSPA-EXPR-YRTYPE-1                 PIC 9(4).
               05  QSPA-EXPR-INTMETH-1                PIC X.
               05  QSPA-EXPR-RATE-2                   PIC 999V999.
               05  QSPA-EXPR-BREAK-2                  PIC 9(7)V99.
               05  QSPA-EXPR-YRTYPE-2                 PIC 9(4).
               05  QSPA-EXPR-INTMETH-2                PIC X.
               05  QSPA-EXPR-RATE-3                   PIC 999V999.
               05  QSPA-EXPR-BREAK-3                  PIC 9(7)V99.
               05  QSPA-EXPR-YRTYPE-3                 PIC 9(4).
               05  QSPA-EXPR-INTMETH-3                PIC X.
               05  QSPA-EXPR-RATE-4                   PIC 999V999.
               05  QSPA-EXPR-BREAK-4                  PIC 9(7)V99.
               05  QSPA-EXPR-YRTYPE-4                 PIC 9(4).
               05  QSPA-EXPR-INTMETH-4                PIC X.
               05  QSPA-EXPR-RATE-5                   PIC 999V999.
               05  QSPA-EXPR-BREAK-5                  PIC 9(7)V99.
               05  QSPA-EXPR-YRTYPE-5                 PIC 9(4).
               05  QSPA-EXPR-INTMETH-5                PIC X.
               05  QSPA-EXPR-LCHG                     PIC X.
               05  QSPA-JUDGFRMLA                     PIC X.
               05  QSPA-JUDGRATE                      PIC 999V999.
               05  QSPA-JUDGYRTYPE                    PIC 9(4).
               05  QSPA-JUDGINTMETH                   PIC X.
               05  QSPA-ACC-FRMLA                     PIC X.
               05  QSPA-ACC-YRTYPE                    PIC 9(4).
               05  QSPA-ACC-INTMETH                   PIC X.
               05  QSPA-ACC-RATE-1                    PIC 999V999.
               05  QSPA-ACC-BREAK-1                   PIC 9(7)V99.
               05  QSPA-ACC-RATE-2                    PIC 999V999.
               05  QSPA-ACC-BREAK-2                   PIC 9(7)V99.
               05  QSPA-ACC-RATE-3                    PIC 999V999.
               05  QSPA-ACC-BREAK-3                   PIC 9(7)V99.
               05  QSPA-ACC-RATE-4                    PIC 999V999.
               05  QSPA-ACC-BREAK-4                   PIC 9(7)V99.
               05  QSPA-ACC-RATE-5                    PIC 999V999.
               05  QSPA-ACC-BREAK-5                   PIC 9(7)V99.
               05  QSPA-1098FRMLA                     PIC X.
               05  QSPA-DISFRMLA                      PIC X.
               05  QSPA-CONTRFRMLA                    PIC X.
               05  QSPA-BANKRULE                      PIC X.
               05  QSPA-CONTPDTH                      PIC 9(4).
               05  QSPA-FINCHG                        PIC 99.
               05  QSPA-BANKUNITPER-CD                PIC X.
               05  QSPA-TRWBANKRULE                   PIC X.
               05  QSPA-TRWCONTPDTH                   PIC 9(4).
               05  QSPA-TRWBANKUNITPER-CD             PIC X.
               05  QSPA-RECFRMLA                      PIC X.
               05  QSPA-RECPAYCD                      PIC X.
               05  QSPA-RECPDTH                       PIC 9(3).
               05  QSPA-DISFRMLA-SPAY-FG              PIC X.
               05  QSPA-ERNFRMLA-1                    PIC 99.
               05  QSPA-ERNAMT-1                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-1                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-1                    PIC X.
               05  QSPA-ERNACCRL-FG-1                 PIC X.
               05  QSPA-ERNACCRL-START-CD-1           PIC X.
               05  QSPA-ERNACCRL-DFCD-1               PIC X.
               05  QSPA-ERN-DAYS-1                    PIC 99.
               05  QSPA-ERN-TERM-1                    PIC 999.
               05  QSPA-ERN-TYPE-1                    PIC X.
               05  QSPA-ERNFRMLA-2                    PIC 99.
               05  QSPA-ERNAMT-2                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-2                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-2                    PIC X.
               05  QSPA-ERNACCRL-FG-2                 PIC X.
               05  QSPA-ERNACCRL-START-CD-2           PIC X.
               05  QSPA-ERNACCRL-DFCD-2               PIC X.
               05  QSPA-ERN-DAYS-2                    PIC 99.
               05  QSPA-ERN-TERM-2                    PIC 999.
               05  QSPA-ERN-TYPE-2                    PIC X.
               05  QSPA-ERNFRMLA-3                    PIC 99.
               05  QSPA-ERNAMT-3                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-3                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-3                    PIC X.
               05  QSPA-ERNACCRL-FG-3                 PIC X.
               05  QSPA-ERNACCRL-START-CD-3           PIC X.
               05  QSPA-ERNACCRL-DFCD-3               PIC X.
               05  QSPA-ERN-DAYS-3                    PIC 99.
               05  QSPA-ERN-TERM-3                    PIC 999.
               05  QSPA-ERN-TYPE-3                    PIC X.
               05  QSPA-ERNFRMLA-4                    PIC 99.
               05  QSPA-ERNAMT-4                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-4                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-4                    PIC X.
               05  QSPA-ERNACCRL-FG-4                 PIC X.
               05  QSPA-ERNACCRL-START-CD-4           PIC X.
               05  QSPA-ERNACCRL-DFCD-4               PIC X.
               05  QSPA-ERN-DAYS-4                    PIC 99.
               05  QSPA-ERN-TERM-4                    PIC 999.
               05  QSPA-ERN-TYPE-4                    PIC X.
               05  QSPA-ERNFRMLA-5                    PIC 99.
               05  QSPA-ERNAMT-5                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-5                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-5                    PIC X.
               05  QSPA-ERNACCRL-FG-5                 PIC X.
               05  QSPA-ERNACCRL-START-CD-5           PIC X.
               05  QSPA-ERNACCRL-DFCD-5               PIC X.
               05  QSPA-ERN-DAYS-5                    PIC 99.
               05  QSPA-ERN-TERM-5                    PIC 999.
               05  QSPA-ERN-TYPE-5                    PIC X.
               05  QSPA-ERNFRMLA-6                    PIC 99.
               05  QSPA-ERNAMT-6                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-6                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-6                    PIC X.
               05  QSPA-ERNACCRL-FG-6                 PIC X.
               05  QSPA-ERNACCRL-START-CD-6           PIC X.
               05  QSPA-ERNACCRL-DFCD-6               PIC X.
               05  QSPA-ERN-DAYS-6                    PIC 99.
               05  QSPA-ERN-TERM-6                    PIC 999.
               05  QSPA-ERN-TYPE-6                    PIC X.
               05  QSPA-ERNFRMLA-7                    PIC 99.
               05  QSPA-ERNAMT-7                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-7                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-7                    PIC X.
               05  QSPA-ERNACCRL-FG-7                 PIC X.
               05  QSPA-ERNACCRL-START-CD-7           PIC X.
               05  QSPA-ERNACCRL-DFCD-7               PIC X.
               05  QSPA-ERN-DAYS-7                    PIC 99.
               05  QSPA-ERN-TERM-7                    PIC 999.
               05  QSPA-ERN-TYPE-7                    PIC X.
               05  QSPA-ERNFRMLA-8                    PIC 99.
               05  QSPA-ERNAMT-8                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-8                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-8                    PIC X.
               05  QSPA-ERNACCRL-FG-8                 PIC X.
               05  QSPA-ERNACCRL-START-CD-8           PIC X.
               05  QSPA-ERNACCRL-DFCD-8               PIC X.
               05  QSPA-ERN-DAYS-8                    PIC 99.
               05  QSPA-ERN-TERM-8                    PIC 999.
               05  QSPA-ERN-TYPE-8                    PIC X.
               05  QSPA-ERNFRMLA-9                    PIC 99.
               05  QSPA-ERNAMT-9                      PIC 9(7)V99.
               05  QSPA-ERNPERCT-9                    PIC 9(3)V99.
               05  QSPA-ERNPERCD-9                    PIC X.
               05  QSPA-ERNACCRL-FG-9                 PIC X.
               05  QSPA-ERNACCRL-START-CD-9           PIC X.
               05  QSPA-ERNACCRL-DFCD-9               PIC X.
               05  QSPA-ERN-DAYS-9                    PIC 99.
               05  QSPA-ERN-TERM-9                    PIC 999.
               05  QSPA-ERN-TYPE-9                    PIC X.
               05  QSPA-ERNFRMLA-10                   PIC 99.
               05  QSPA-ERNAMT-10                     PIC 9(7)V99.
               05  QSPA-ERNPERCT-10                   PIC 9(3)V99.
               05  QSPA-ERNPERCD-10                   PIC X.
               05  QSPA-ERNACCRL-FG-10                PIC X.
               05  QSPA-ERNACCRL-START-CD-10          PIC X.
               05  QSPA-ERNACCRL-DFCD-10              PIC X.
               05  QSPA-ERN-DAYS-10                   PIC 99.
               05  QSPA-ERN-TERM-10                   PIC 999.
               05  QSPA-ERN-TYPE-10                   PIC X.
               05  QSPA-ERNACCEL-CD                   PIC X.
               05  QSPA-ERNJD-CD                      PIC X.
               05  QSPA-MIDSTRERNCD                   PIC X.
               05  QSPA-EARN-ORDER-1                  PIC 99.
               05  QSPA-EARN-ORDER-2                  PIC 99.
               05  QSPA-EARN-ORDER-3                  PIC 99.
               05  QSPA-EARN-ORDER-4                  PIC 99.
               05  QSPA-EARN-ORDER-5                  PIC 99.
               05  QSPA-EARN-ORDER-6                  PIC 99.
               05  QSPA-EARN-ORDER-7                  PIC 99.
               05  QSPA-EARN-ORDER-8                  PIC 99.
               05  QSPA-EARN-ORDER-9                  PIC 99.
               05  QSPA-EARN-ORDER-10                 PIC 99.
               05  QSPA-ERNDELQFRMLA                  PIC X.
               05  QSPA-ERNDELQ-START                 PIC 999.
               05  QSPA-ERNDELQ-STOP                  PIC 999.
               05  QSPA-1098-INC-LC                   PIC X.
               05  QSPA-MAX-IB-ACR-DAYS               PIC 999.
               05  QSPA-LCGRACE                       PIC 9(2).
               05  QSPA-LCGRACECD                     PIC X.
               05  QSPA-LCTYPE                        PIC X(2).
               05  QSPA-LCRATE                        PIC 9(4)V9(4).
               05  QSPA-LCFRMLA                       PIC X(2).
               05  QSPA-LCMINCHG                      PIC 99V99.
               05  QSPA-LCMAXAMT                      PIC 9(4)V99.
               05  QSPA-LCDELFAC                      PIC 9(3).
               05  QSPA-LCYRTYPE                      PIC 9(4).
               05  QSPA-LC-CODE                       PIC 99.
               05  QSPA-LCUNITPER-CD                  PIC X.
               05  QSPA-DEFFRMLA                      PIC 99.
               05  QSPA-DEFEXCEPT                     PIC X.
               05  QSPA-DEFWORKER                     PIC 99V9999.
               05  QSPA-DEFPERCD                      PIC 99.
               05  QSPA-DEFBASE                       PIC 99.
               05  QSPA-DEFPOLCD                      PIC X.
               05  QSPA-DEFPOLMX-1                    PIC 99.
               05  QSPA-DEFPOLMX-2                    PIC 99.
               05  QSPA-DEFPOLMX-3                    PIC 99.
               05  QSPA-DEFMIN                        PIC 99V99.
               05  QSPA-DEFMAX                        PIC 9999V99.
               05  QSPA-DEFUNITPER-CD                 PIC X.
               05  QSPA-PRFORM-1                      PIC 9(4).
               05  QSPA-PRFLAG-1                      PIC X.
               05  QSPA-PRFORM-2                      PIC 9(4).
               05  QSPA-PRFLAG-2                      PIC X.
               05  QSPA-PRFORM-3                      PIC 9(4).
               05  QSPA-PRFLAG-3                      PIC X.
               05  QSPA-PRFORM-4                      PIC 9(4).
               05  QSPA-PRFLAG-4                      PIC X.
               05  QSPA-PRFORM-5                      PIC 9(4).
               05  QSPA-PRFLAG-5                      PIC X.
               05  QSPA-PRFORM-6                      PIC 9(4).
               05  QSPA-PRFLAG-6                      PIC X.
               05  QSPA-PRFORM-7                      PIC 9(4).
               05  QSPA-PRFLAG-7                      PIC X.
               05  QSPA-PRFORM-8                      PIC 9(4).
               05  QSPA-PRFLAG-8                      PIC X.
               05  QSPA-PRFORM-9                      PIC 9(4).
               05  QSPA-PRFLAG-9                      PIC X.
               05  QSPA-PRFORM-10                     PIC 9(4).
               05  QSPA-PRFLAG-10                     PIC X.
               05  QSPA-PRFORM-11                     PIC 9(4).
               05  QSPA-PRFLAG-11                     PIC X.
               05  QSPA-PRFORM-12                     PIC 9(4).
               05  QSPA-PRFLAG-12                     PIC X.
               05  QSPA-PRFORM-13                     PIC 9(4).
               05  QSPA-PRFLAG-13                     PIC X.
               05  QSPA-PRFORM-14                     PIC 9(4).
               05  QSPA-PRFLAG-14                     PIC X.
               05  QSPA-PRFORM-15                     PIC 9(4).
               05  QSPA-PRFLAG-15                     PIC X.
               05  QSPA-PRFORM-16                     PIC 9(4).
               05  QSPA-PRFLAG-16                     PIC X.
               05  QSPA-PRFORM-17                     PIC 9(4).
               05  QSPA-PRFLAG-17                     PIC X.
               05  QSPA-PRFORM-18                     PIC 9(4).
               05  QSPA-PRFLAG-18                     PIC X.
               05  QSPA-PRFORM-19                     PIC 9(4).
               05  QSPA-PRFLAG-19                     PIC X.
               05  QSPA-PRFORM-20                     PIC 9(4).
               05  QSPA-PRFLAG-20                     PIC X.
               05  QSPA-IMED-REC                      PIC X.
               05  QSPA-SCFRMLA                       PIC 99.
               05  QSPA-SCRATE                        PIC 99V99.
               05  QSPA-SCMIN                         PIC 9(4)V99.
               05  QSPA-SCMAX                         PIC 9(6)V99.
               05  QSPA-MFFRMLA                       PIC 99.
               05  QSPA-MFRATE                        PIC 99V99.
               05  QSPA-MFMIN                         PIC 9(4)V99.
               05  QSPA-MFMAX                         PIC 9(5)V99.
               05  QSPA-INTMETHOD                     PIC X.
               05  QSPA-YEARTYPE                      PIC 9(4).
               05  QSPA-PYEXFRMLA                     PIC 99.
               05  QSPA-PYEXSPCODE                    PIC 99.
               05  QSPA-PYEXYEARTP                    PIC 9(4).
               05  QSPA-PYEXFMDATE                    PIC X.
               05  QSPA-PYEXBASECD                    PIC X.
               05  QSPA-PYEXBASE                      PIC 9(4).
               05  QSPA-PYEXDAYS                      PIC 9(4).
               05  QSPA-CAL-SPFRMLA                   PIC 99.
               05  QSPA-CAL-RATETYPE-1                PIC X.
               05  QSPA-CAL-BREAKTYPE-1               PIC X.
               05  QSPA-CAL-CHGABLE-1                 PIC X.
               05  QSPA-CAL-NORATES-1                 PIC S9.
               05  QSPA-CAL-TBFRMLA-1                 PIC 99.
               05  QSPA-CAL-RATE-1-1                  PIC 999V999.
               05  QSPA-CAL-BREAK-1-1                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-1-2                  PIC 999V999.
               05  QSPA-CAL-BREAK-1-2                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-1-3                  PIC 999V999.
               05  QSPA-CAL-BREAK-1-3                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-1-4                  PIC 999V999.
               05  QSPA-CAL-BREAK-1-4                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-1-5                  PIC 999V999.
               05  QSPA-CAL-BREAK-1-5                 PIC 9(6)V99.
               05  QSPA-CAL-RATETYPE-2                PIC X.
               05  QSPA-CAL-BREAKTYPE-2               PIC X.
               05  QSPA-CAL-CHGABLE-2                 PIC X.
               05  QSPA-CAL-NORATES-2                 PIC S9.
               05  QSPA-CAL-TBFRMLA-2                 PIC 99.
               05  QSPA-CAL-RATE-2-1                  PIC 999V999.
               05  QSPA-CAL-BREAK-2-1                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-2-2                  PIC 999V999.
               05  QSPA-CAL-BREAK-2-2                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-2-3                  PIC 999V999.
               05  QSPA-CAL-BREAK-2-3                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-2-4                  PIC 999V999.
               05  QSPA-CAL-BREAK-2-4                 PIC 9(6)V99.
               05  QSPA-CAL-RATE-2-5                  PIC 999V999.
               05  QSPA-CAL-BREAK-2-5                 PIC 9(6)V99.
               05  QSPA-MINSF-FRMLA                   PIC 99.
               05  QSPA-MINSF-CHG                     PIC 9(6)V99.
               05  QSPA-MAXAPR-FRMLA                  PIC 99.
               05  QSPA-EDT-NOEDITS-1                 PIC S9.
               05  QSPA-EDT-TYPE-1-1                  PIC X.
               05  QSPA-EDT-HL-1-1                    PIC X.
               05  QSPA-EDT-COLL-1-1                  PIC XX.
               05  QSPA-EDT-CD-1-1                    PIC X.
               05  QSPA-EDT-RATE-1-1                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-1-1              PIC X.
               05  QSPA-EDT-AMT-1-1                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-1-1                PIC X.
               05  QSPA-EDT-TERM-1-1                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-1-2                  PIC X.
               05  QSPA-EDT-HL-1-2                    PIC X.
               05  QSPA-EDT-COLL-1-2                  PIC XX.
               05  QSPA-EDT-CD-1-2                    PIC X.
               05  QSPA-EDT-RATE-1-2                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-1-2              PIC X.
               05  QSPA-EDT-AMT-1-2                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-1-2                PIC X.
               05  QSPA-EDT-TERM-1-2                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-1-3                  PIC X.
               05  QSPA-EDT-HL-1-3                    PIC X.
               05  QSPA-EDT-COLL-1-3                  PIC XX.
               05  QSPA-EDT-CD-1-3                    PIC X.
               05  QSPA-EDT-RATE-1-3                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-1-3              PIC X.
               05  QSPA-EDT-AMT-1-3                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-1-3                PIC X.
               05  QSPA-EDT-TERM-1-3                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-1-4                  PIC X.
               05  QSPA-EDT-HL-1-4                    PIC X.
               05  QSPA-EDT-COLL-1-4                  PIC XX.
               05  QSPA-EDT-CD-1-4                    PIC X.
               05  QSPA-EDT-RATE-1-4                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-1-4              PIC X.
               05  QSPA-EDT-AMT-1-4                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-1-4                PIC X.
               05  QSPA-EDT-TERM-1-4                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-1-5                  PIC X.
               05  QSPA-EDT-HL-1-5                    PIC X.
               05  QSPA-EDT-COLL-1-5                  PIC XX.
               05  QSPA-EDT-CD-1-5                    PIC X.
               05  QSPA-EDT-RATE-1-5                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-1-5              PIC X.
               05  QSPA-EDT-AMT-1-5                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-1-5                PIC X.
               05  QSPA-EDT-TERM-1-5                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-1-6                  PIC X.
               05  QSPA-EDT-HL-1-6                    PIC X.
               05  QSPA-EDT-COLL-1-6                  PIC XX.
               05  QSPA-EDT-CD-1-6                    PIC X.
               05  QSPA-EDT-RATE-1-6                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-1-6              PIC X.
               05  QSPA-EDT-AMT-1-6                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-1-6                PIC X.
               05  QSPA-EDT-TERM-1-6                  PIC 9(3)V9.
               05  QSPA-EDT-NOEDITS-2                 PIC S9.
               05  QSPA-EDT-TYPE-2-1                  PIC X.
               05  QSPA-EDT-HL-2-1                    PIC X.
               05  QSPA-EDT-COLL-2-1                  PIC XX.
               05  QSPA-EDT-CD-2-1                    PIC X.
               05  QSPA-EDT-RATE-2-1                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-2-1              PIC X.
               05  QSPA-EDT-AMT-2-1                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-2-1                PIC X.
               05  QSPA-EDT-TERM-2-1                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-2-2                  PIC X.
               05  QSPA-EDT-HL-2-2                    PIC X.
               05  QSPA-EDT-COLL-2-2                  PIC XX.
               05  QSPA-EDT-CD-2-2                    PIC X.
               05  QSPA-EDT-RATE-2-2                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-2-2              PIC X.
               05  QSPA-EDT-AMT-2-2                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-2-2                PIC X.
               05  QSPA-EDT-TERM-2-2                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-2-3                  PIC X.
               05  QSPA-EDT-HL-2-3                    PIC X.
               05  QSPA-EDT-COLL-2-3                  PIC XX.
               05  QSPA-EDT-CD-2-3                    PIC X.
               05  QSPA-EDT-RATE-2-3                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-2-3              PIC X.
               05  QSPA-EDT-AMT-2-3                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-2-3                PIC X.
               05  QSPA-EDT-TERM-2-3                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-2-4                  PIC X.
               05  QSPA-EDT-HL-2-4                    PIC X.
               05  QSPA-EDT-COLL-2-4                  PIC XX.
               05  QSPA-EDT-CD-2-4                    PIC X.
               05  QSPA-EDT-RATE-2-4                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-2-4              PIC X.
               05  QSPA-EDT-AMT-2-4                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-2-4                PIC X.
               05  QSPA-EDT-TERM-2-4                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-2-5                  PIC X.
               05  QSPA-EDT-HL-2-5                    PIC X.
               05  QSPA-EDT-COLL-2-5                  PIC XX.
               05  QSPA-EDT-CD-2-5                    PIC X.
               05  QSPA-EDT-RATE-2-5                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-2-5              PIC X.
               05  QSPA-EDT-AMT-2-5                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-2-5                PIC X.
               05  QSPA-EDT-TERM-2-5                  PIC 9(3)V9.
               05  QSPA-EDT-TYPE-2-6                  PIC X.
               05  QSPA-EDT-HL-2-6                    PIC X.
               05  QSPA-EDT-COLL-2-6                  PIC XX.
               05  QSPA-EDT-CD-2-6                    PIC X.
               05  QSPA-EDT-RATE-2-6                  PIC 99V99.
               05  QSPA-EDT-RATETYPE-2-6              PIC X.
               05  QSPA-EDT-AMT-2-6                   PIC 9(7)V99.
               05  QSPA-EDT-AMTYPE-2-6                PIC X.
               05  QSPA-EDT-TERM-2-6                  PIC 9(3)V9.
           03  QSPB-REC.
               05  QSPB-ORGST                         PIC XX.
               05  QSPB-SPRCLASS                      PIC 999.
               05  QSPB-SUBCLASS                      PIC 99.
               05  QSPB-LAWCODE                       PIC 99.
               05  QSPB-INSCOM-1                      PIC 99.
               05  QSPB-INSRATECD-1                   PIC 999.
               05  QSPB-INSJFAC-1                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-1                   PIC 99V99.
               05  QSPB-INSCHGABL-1                   PIC X(2).
               05  QSPB-INSTERM-1-1                   PIC 9(3).
               05  QSPB-INSRATE-1-1                   PIC 99V9999.
               05  QSPB-INSTERM-1-2                   PIC 9(3).
               05  QSPB-INSRATE-1-2                   PIC 99V9999.
               05  QSPB-INSTERM-1-3                   PIC 9(3).
               05  QSPB-INSRATE-1-3                   PIC 99V9999.
               05  QSPB-INSTERM-1-4                   PIC 9(3).
               05  QSPB-INSRATE-1-4                   PIC 99V9999.
               05  QSPB-INSTERM-1-5                   PIC 9(3).
               05  QSPB-INSRATE-1-5                   PIC 99V9999.
               05  QSPB-INSTERM-1-6                   PIC 9(3).
               05  QSPB-INSRATE-1-6                   PIC 99V9999.
               05  QSPB-INSTERM-1-7                   PIC 9(3).
               05  QSPB-INSRATE-1-7                   PIC 99V9999.
               05  QSPB-INSTERM-1-8                   PIC 9(3).
               05  QSPB-INSRATE-1-8                   PIC 99V9999.
               05  QSPB-INSTERM-1-9                   PIC 9(3).
               05  QSPB-INSRATE-1-9                   PIC 99V9999.
               05  QSPB-INSTERM-1-10                  PIC 9(3).
               05  QSPB-INSRATE-1-10                  PIC 99V9999.
               05  QSPB-INSTERM-1-11                  PIC 9(3).
               05  QSPB-INSRATE-1-11                  PIC 99V9999.
               05  QSPB-INSTERM-1-12                  PIC 9(3).
               05  QSPB-INSRATE-1-12                  PIC 99V9999.
               05  QSPB-INSTERM-1-13                  PIC 9(3).
               05  QSPB-INSRATE-1-13                  PIC 99V9999.
               05  QSPB-INSTERM-1-14                  PIC 9(3).
               05  QSPB-INSRATE-1-14                  PIC 99V9999.
               05  QSPB-INSTERM-1-15                  PIC 9(3).
               05  QSPB-INSRATE-1-15                  PIC 99V9999.
               05  QSPB-INSTERM-1-16                  PIC 9(3).
               05  QSPB-INSRATE-1-16                  PIC 99V9999.
               05  QSPB-INSTERM-1-17                  PIC 9(3).
               05  QSPB-INSRATE-1-17                  PIC 99V9999.
               05  QSPB-INSTERM-1-18                  PIC 9(3).
               05  QSPB-INSRATE-1-18                  PIC 99V9999.
               05  QSPB-INSTERM-1-19                  PIC 9(3).
               05  QSPB-INSRATE-1-19                  PIC 99V9999.
               05  QSPB-INSTERM-1-20                  PIC 9(3).
               05  QSPB-INSRATE-1-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-1                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-1                PIC 9(6).
               05  QSPB-INSMAXCHGABL-1                PIC 9(7).
               05  QSPB-INSMAXBENIF-1                 PIC 9(7).
               05  QSPB-INSMAXTERM-1                  PIC 9(3).
               05  QSPB-INSCHGBEN-1                   PIC X.
               05  QSPB-INSEXCEPT-1                   PIC 99.
               05  QSPB-INSNOVER-1                    PIC 99.
               05  QSPB-INSMINAGE-1-1                 PIC 99.
               05  QSPB-INSMAXAGE-1-1                 PIC 99.
               05  QSPB-INSMAXCOV-1-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-1-1                 PIC 9(3).
               05  QSPB-INSMINAGE-1-2                 PIC 99.
               05  QSPB-INSMAXAGE-1-2                 PIC 99.
               05  QSPB-INSMAXCOV-1-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-1-2                 PIC 9(3).
               05  QSPB-INSMINAGE-1-3                 PIC 99.
               05  QSPB-INSMAXAGE-1-3                 PIC 99.
               05  QSPB-INSMAXCOV-1-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-1-3                 PIC 9(3).
               05  QSPB-INSMINAGE-1-4                 PIC 99.
               05  QSPB-INSMAXAGE-1-4                 PIC 99.
               05  QSPB-INSMAXCOV-1-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-1-4                 PIC 9(3).
               05  QSPB-INSMINAGE-1-5                 PIC 99.
               05  QSPB-INSMAXAGE-1-5                 PIC 99.
               05  QSPB-INSMAXCOV-1-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-1-5                 PIC 9(3).
               05  QSPB-INSFEECD-1                    PIC XX.
               05  QSPB-INSFEE-FRMLA-1                PIC X.
               05  QSPB-INSCHGABL-FG-1                PIC X.
               05  QSPB-INSREJECT-FG-1                PIC X.
               05  QSPB-INSFINCHG-1                   PIC X.
               05  QSPB-INSCOM-2                      PIC 99.
               05  QSPB-INSRATECD-2                   PIC 999.
               05  QSPB-INSJFAC-2                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-2                   PIC 99V99.
               05  QSPB-INSCHGABL-2                   PIC X(2).
               05  QSPB-INSTERM-2-1                   PIC 9(3).
               05  QSPB-INSRATE-2-1                   PIC 99V9999.
               05  QSPB-INSTERM-2-2                   PIC 9(3).
               05  QSPB-INSRATE-2-2                   PIC 99V9999.
               05  QSPB-INSTERM-2-3                   PIC 9(3).
               05  QSPB-INSRATE-2-3                   PIC 99V9999.
               05  QSPB-INSTERM-2-4                   PIC 9(3).
               05  QSPB-INSRATE-2-4                   PIC 99V9999.
               05  QSPB-INSTERM-2-5                   PIC 9(3).
               05  QSPB-INSRATE-2-5                   PIC 99V9999.
               05  QSPB-INSTERM-2-6                   PIC 9(3).
               05  QSPB-INSRATE-2-6                   PIC 99V9999.
               05  QSPB-INSTERM-2-7                   PIC 9(3).
               05  QSPB-INSRATE-2-7                   PIC 99V9999.
               05  QSPB-INSTERM-2-8                   PIC 9(3).
               05  QSPB-INSRATE-2-8                   PIC 99V9999.
               05  QSPB-INSTERM-2-9                   PIC 9(3).
               05  QSPB-INSRATE-2-9                   PIC 99V9999.
               05  QSPB-INSTERM-2-10                  PIC 9(3).
               05  QSPB-INSRATE-2-10                  PIC 99V9999.
               05  QSPB-INSTERM-2-11                  PIC 9(3).
               05  QSPB-INSRATE-2-11                  PIC 99V9999.
               05  QSPB-INSTERM-2-12                  PIC 9(3).
               05  QSPB-INSRATE-2-12                  PIC 99V9999.
               05  QSPB-INSTERM-2-13                  PIC 9(3).
               05  QSPB-INSRATE-2-13                  PIC 99V9999.
               05  QSPB-INSTERM-2-14                  PIC 9(3).
               05  QSPB-INSRATE-2-14                  PIC 99V9999.
               05  QSPB-INSTERM-2-15                  PIC 9(3).
               05  QSPB-INSRATE-2-15                  PIC 99V9999.
               05  QSPB-INSTERM-2-16                  PIC 9(3).
               05  QSPB-INSRATE-2-16                  PIC 99V9999.
               05  QSPB-INSTERM-2-17                  PIC 9(3).
               05  QSPB-INSRATE-2-17                  PIC 99V9999.
               05  QSPB-INSTERM-2-18                  PIC 9(3).
               05  QSPB-INSRATE-2-18                  PIC 99V9999.
               05  QSPB-INSTERM-2-19                  PIC 9(3).
               05  QSPB-INSRATE-2-19                  PIC 99V9999.
               05  QSPB-INSTERM-2-20                  PIC 9(3).
               05  QSPB-INSRATE-2-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-2                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-2                PIC 9(6).
               05  QSPB-INSMAXCHGABL-2                PIC 9(7).
               05  QSPB-INSMAXBENIF-2                 PIC 9(7).
               05  QSPB-INSMAXTERM-2                  PIC 9(3).
               05  QSPB-INSCHGBEN-2                   PIC X.
               05  QSPB-INSEXCEPT-2                   PIC 99.
               05  QSPB-INSNOVER-2                    PIC 99.
               05  QSPB-INSMINAGE-2-1                 PIC 99.
               05  QSPB-INSMAXAGE-2-1                 PIC 99.
               05  QSPB-INSMAXCOV-2-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-2-1                 PIC 9(3).
               05  QSPB-INSMINAGE-2-2                 PIC 99.
               05  QSPB-INSMAXAGE-2-2                 PIC 99.
               05  QSPB-INSMAXCOV-2-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-2-2                 PIC 9(3).
               05  QSPB-INSMINAGE-2-3                 PIC 99.
               05  QSPB-INSMAXAGE-2-3                 PIC 99.
               05  QSPB-INSMAXCOV-2-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-2-3                 PIC 9(3).
               05  QSPB-INSMINAGE-2-4                 PIC 99.
               05  QSPB-INSMAXAGE-2-4                 PIC 99.
               05  QSPB-INSMAXCOV-2-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-2-4                 PIC 9(3).
               05  QSPB-INSMINAGE-2-5                 PIC 99.
               05  QSPB-INSMAXAGE-2-5                 PIC 99.
               05  QSPB-INSMAXCOV-2-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-2-5                 PIC 9(3).
               05  QSPB-INSFEECD-2                    PIC XX.
               05  QSPB-INSFEE-FRMLA-2                PIC X.
               05  QSPB-INSCHGABL-FG-2                PIC X.
               05  QSPB-INSREJECT-FG-2                PIC X.
               05  QSPB-INSFINCHG-2                   PIC X.
               05  QSPB-INSCOM-3                      PIC 99.
               05  QSPB-INSRATECD-3                   PIC 999.
               05  QSPB-INSJFAC-3                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-3                   PIC 99V99.
               05  QSPB-INSCHGABL-3                   PIC X(2).
               05  QSPB-INSTERM-3-1                   PIC 9(3).
               05  QSPB-INSRATE-3-1                   PIC 99V9999.
               05  QSPB-INSTERM-3-2                   PIC 9(3).
               05  QSPB-INSRATE-3-2                   PIC 99V9999.
               05  QSPB-INSTERM-3-3                   PIC 9(3).
               05  QSPB-INSRATE-3-3                   PIC 99V9999.
               05  QSPB-INSTERM-3-4                   PIC 9(3).
               05  QSPB-INSRATE-3-4                   PIC 99V9999.
               05  QSPB-INSTERM-3-5                   PIC 9(3).
               05  QSPB-INSRATE-3-5                   PIC 99V9999.
               05  QSPB-INSTERM-3-6                   PIC 9(3).
               05  QSPB-INSRATE-3-6                   PIC 99V9999.
               05  QSPB-INSTERM-3-7                   PIC 9(3).
               05  QSPB-INSRATE-3-7                   PIC 99V9999.
               05  QSPB-INSTERM-3-8                   PIC 9(3).
               05  QSPB-INSRATE-3-8                   PIC 99V9999.
               05  QSPB-INSTERM-3-9                   PIC 9(3).
               05  QSPB-INSRATE-3-9                   PIC 99V9999.
               05  QSPB-INSTERM-3-10                  PIC 9(3).
               05  QSPB-INSRATE-3-10                  PIC 99V9999.
               05  QSPB-INSTERM-3-11                  PIC 9(3).
               05  QSPB-INSRATE-3-11                  PIC 99V9999.
               05  QSPB-INSTERM-3-12                  PIC 9(3).
               05  QSPB-INSRATE-3-12                  PIC 99V9999.
               05  QSPB-INSTERM-3-13                  PIC 9(3).
               05  QSPB-INSRATE-3-13                  PIC 99V9999.
               05  QSPB-INSTERM-3-14                  PIC 9(3).
               05  QSPB-INSRATE-3-14                  PIC 99V9999.
               05  QSPB-INSTERM-3-15                  PIC 9(3).
               05  QSPB-INSRATE-3-15                  PIC 99V9999.
               05  QSPB-INSTERM-3-16                  PIC 9(3).
               05  QSPB-INSRATE-3-16                  PIC 99V9999.
               05  QSPB-INSTERM-3-17                  PIC 9(3).
               05  QSPB-INSRATE-3-17                  PIC 99V9999.
               05  QSPB-INSTERM-3-18                  PIC 9(3).
               05  QSPB-INSRATE-3-18                  PIC 99V9999.
               05  QSPB-INSTERM-3-19                  PIC 9(3).
               05  QSPB-INSRATE-3-19                  PIC 99V9999.
               05  QSPB-INSTERM-3-20                  PIC 9(3).
               05  QSPB-INSRATE-3-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-3                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-3                PIC 9(6).
               05  QSPB-INSMAXCHGABL-3                PIC 9(7).
               05  QSPB-INSMAXBENIF-3                 PIC 9(7).
               05  QSPB-INSMAXTERM-3                  PIC 9(3).
               05  QSPB-INSCHGBEN-3                   PIC X.
               05  QSPB-INSEXCEPT-3                   PIC 99.
               05  QSPB-INSNOVER-3                    PIC 99.
               05  QSPB-INSMINAGE-3-1                 PIC 99.
               05  QSPB-INSMAXAGE-3-1                 PIC 99.
               05  QSPB-INSMAXCOV-3-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-3-1                 PIC 9(3).
               05  QSPB-INSMINAGE-3-2                 PIC 99.
               05  QSPB-INSMAXAGE-3-2                 PIC 99.
               05  QSPB-INSMAXCOV-3-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-3-2                 PIC 9(3).
               05  QSPB-INSMINAGE-3-3                 PIC 99.
               05  QSPB-INSMAXAGE-3-3                 PIC 99.
               05  QSPB-INSMAXCOV-3-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-3-3                 PIC 9(3).
               05  QSPB-INSMINAGE-3-4                 PIC 99.
               05  QSPB-INSMAXAGE-3-4                 PIC 99.
               05  QSPB-INSMAXCOV-3-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-3-4                 PIC 9(3).
               05  QSPB-INSMINAGE-3-5                 PIC 99.
               05  QSPB-INSMAXAGE-3-5                 PIC 99.
               05  QSPB-INSMAXCOV-3-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-3-5                 PIC 9(3).
               05  QSPB-INSFEECD-3                    PIC XX.
               05  QSPB-INSFEE-FRMLA-3                PIC X.
               05  QSPB-INSCHGABL-FG-3                PIC X.
               05  QSPB-INSREJECT-FG-3                PIC X.
               05  QSPB-INSFINCHG-3                   PIC X.
               05  QSPB-INSCOM-4                      PIC 99.
               05  QSPB-INSRATECD-4                   PIC 999.
               05  QSPB-INSJFAC-4                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-4                   PIC 99V99.
               05  QSPB-INSCHGABL-4                   PIC X(2).
               05  QSPB-INSTERM-4-1                   PIC 9(3).
               05  QSPB-INSRATE-4-1                   PIC 99V9999.
               05  QSPB-INSTERM-4-2                   PIC 9(3).
               05  QSPB-INSRATE-4-2                   PIC 99V9999.
               05  QSPB-INSTERM-4-3                   PIC 9(3).
               05  QSPB-INSRATE-4-3                   PIC 99V9999.
               05  QSPB-INSTERM-4-4                   PIC 9(3).
               05  QSPB-INSRATE-4-4                   PIC 99V9999.
               05  QSPB-INSTERM-4-5                   PIC 9(3).
               05  QSPB-INSRATE-4-5                   PIC 99V9999.
               05  QSPB-INSTERM-4-6                   PIC 9(3).
               05  QSPB-INSRATE-4-6                   PIC 99V9999.
               05  QSPB-INSTERM-4-7                   PIC 9(3).
               05  QSPB-INSRATE-4-7                   PIC 99V9999.
               05  QSPB-INSTERM-4-8                   PIC 9(3).
               05  QSPB-INSRATE-4-8                   PIC 99V9999.
               05  QSPB-INSTERM-4-9                   PIC 9(3).
               05  QSPB-INSRATE-4-9                   PIC 99V9999.
               05  QSPB-INSTERM-4-10                  PIC 9(3).
               05  QSPB-INSRATE-4-10                  PIC 99V9999.
               05  QSPB-INSTERM-4-11                  PIC 9(3).
               05  QSPB-INSRATE-4-11                  PIC 99V9999.
               05  QSPB-INSTERM-4-12                  PIC 9(3).
               05  QSPB-INSRATE-4-12                  PIC 99V9999.
               05  QSPB-INSTERM-4-13                  PIC 9(3).
               05  QSPB-INSRATE-4-13                  PIC 99V9999.
               05  QSPB-INSTERM-4-14                  PIC 9(3).
               05  QSPB-INSRATE-4-14                  PIC 99V9999.
               05  QSPB-INSTERM-4-15                  PIC 9(3).
               05  QSPB-INSRATE-4-15                  PIC 99V9999.
               05  QSPB-INSTERM-4-16                  PIC 9(3).
               05  QSPB-INSRATE-4-16                  PIC 99V9999.
               05  QSPB-INSTERM-4-17                  PIC 9(3).
               05  QSPB-INSRATE-4-17                  PIC 99V9999.
               05  QSPB-INSTERM-4-18                  PIC 9(3).
               05  QSPB-INSRATE-4-18                  PIC 99V9999.
               05  QSPB-INSTERM-4-19                  PIC 9(3).
               05  QSPB-INSRATE-4-19                  PIC 99V9999.
               05  QSPB-INSTERM-4-20                  PIC 9(3).
               05  QSPB-INSRATE-4-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-4                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-4                PIC 9(6).
               05  QSPB-INSMAXCHGABL-4                PIC 9(7).
               05  QSPB-INSMAXBENIF-4                 PIC 9(7).
               05  QSPB-INSMAXTERM-4                  PIC 9(3).
               05  QSPB-INSCHGBEN-4                   PIC X.
               05  QSPB-INSEXCEPT-4                   PIC 99.
               05  QSPB-INSNOVER-4                    PIC 99.
               05  QSPB-INSMINAGE-4-1                 PIC 99.
               05  QSPB-INSMAXAGE-4-1                 PIC 99.
               05  QSPB-INSMAXCOV-4-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-4-1                 PIC 9(3).
               05  QSPB-INSMINAGE-4-2                 PIC 99.
               05  QSPB-INSMAXAGE-4-2                 PIC 99.
               05  QSPB-INSMAXCOV-4-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-4-2                 PIC 9(3).
               05  QSPB-INSMINAGE-4-3                 PIC 99.
               05  QSPB-INSMAXAGE-4-3                 PIC 99.
               05  QSPB-INSMAXCOV-4-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-4-3                 PIC 9(3).
               05  QSPB-INSMINAGE-4-4                 PIC 99.
               05  QSPB-INSMAXAGE-4-4                 PIC 99.
               05  QSPB-INSMAXCOV-4-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-4-4                 PIC 9(3).
               05  QSPB-INSMINAGE-4-5                 PIC 99.
               05  QSPB-INSMAXAGE-4-5                 PIC 99.
               05  QSPB-INSMAXCOV-4-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-4-5                 PIC 9(3).
               05  QSPB-INSFEECD-4                    PIC XX.
               05  QSPB-INSFEE-FRMLA-4                PIC X.
               05  QSPB-INSCHGABL-FG-4                PIC X.
               05  QSPB-INSREJECT-FG-4                PIC X.
               05  QSPB-INSFINCHG-4                   PIC X.
               05  QSPB-INSCOM-5                      PIC 99.
               05  QSPB-INSRATECD-5                   PIC 999.
               05  QSPB-INSJFAC-5                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-5                   PIC 99V99.
               05  QSPB-INSCHGABL-5                   PIC X(2).
               05  QSPB-INSTERM-5-1                   PIC 9(3).
               05  QSPB-INSRATE-5-1                   PIC 99V9999.
               05  QSPB-INSTERM-5-2                   PIC 9(3).
               05  QSPB-INSRATE-5-2                   PIC 99V9999.
               05  QSPB-INSTERM-5-3                   PIC 9(3).
               05  QSPB-INSRATE-5-3                   PIC 99V9999.
               05  QSPB-INSTERM-5-4                   PIC 9(3).
               05  QSPB-INSRATE-5-4                   PIC 99V9999.
               05  QSPB-INSTERM-5-5                   PIC 9(3).
               05  QSPB-INSRATE-5-5                   PIC 99V9999.
               05  QSPB-INSTERM-5-6                   PIC 9(3).
               05  QSPB-INSRATE-5-6                   PIC 99V9999.
               05  QSPB-INSTERM-5-7                   PIC 9(3).
               05  QSPB-INSRATE-5-7                   PIC 99V9999.
               05  QSPB-INSTERM-5-8                   PIC 9(3).
               05  QSPB-INSRATE-5-8                   PIC 99V9999.
               05  QSPB-INSTERM-5-9                   PIC 9(3).
               05  QSPB-INSRATE-5-9                   PIC 99V9999.
               05  QSPB-INSTERM-5-10                  PIC 9(3).
               05  QSPB-INSRATE-5-10                  PIC 99V9999.
               05  QSPB-INSTERM-5-11                  PIC 9(3).
               05  QSPB-INSRATE-5-11                  PIC 99V9999.
               05  QSPB-INSTERM-5-12                  PIC 9(3).
               05  QSPB-INSRATE-5-12                  PIC 99V9999.
               05  QSPB-INSTERM-5-13                  PIC 9(3).
               05  QSPB-INSRATE-5-13                  PIC 99V9999.
               05  QSPB-INSTERM-5-14                  PIC 9(3).
               05  QSPB-INSRATE-5-14                  PIC 99V9999.
               05  QSPB-INSTERM-5-15                  PIC 9(3).
               05  QSPB-INSRATE-5-15                  PIC 99V9999.
               05  QSPB-INSTERM-5-16                  PIC 9(3).
               05  QSPB-INSRATE-5-16                  PIC 99V9999.
               05  QSPB-INSTERM-5-17                  PIC 9(3).
               05  QSPB-INSRATE-5-17                  PIC 99V9999.
               05  QSPB-INSTERM-5-18                  PIC 9(3).
               05  QSPB-INSRATE-5-18                  PIC 99V9999.
               05  QSPB-INSTERM-5-19                  PIC 9(3).
               05  QSPB-INSRATE-5-19                  PIC 99V9999.
               05  QSPB-INSTERM-5-20                  PIC 9(3).
               05  QSPB-INSRATE-5-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-5                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-5                PIC 9(6).
               05  QSPB-INSMAXCHGABL-5                PIC 9(7).
               05  QSPB-INSMAXBENIF-5                 PIC 9(7).
               05  QSPB-INSMAXTERM-5                  PIC 9(3).
               05  QSPB-INSCHGBEN-5                   PIC X.
               05  QSPB-INSEXCEPT-5                   PIC 99.
               05  QSPB-INSNOVER-5                    PIC 99.
               05  QSPB-INSMINAGE-5-1                 PIC 99.
               05  QSPB-INSMAXAGE-5-1                 PIC 99.
               05  QSPB-INSMAXCOV-5-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-5-1                 PIC 9(3).
               05  QSPB-INSMINAGE-5-2                 PIC 99.
               05  QSPB-INSMAXAGE-5-2                 PIC 99.
               05  QSPB-INSMAXCOV-5-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-5-2                 PIC 9(3).
               05  QSPB-INSMINAGE-5-3                 PIC 99.
               05  QSPB-INSMAXAGE-5-3                 PIC 99.
               05  QSPB-INSMAXCOV-5-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-5-3                 PIC 9(3).
               05  QSPB-INSMINAGE-5-4                 PIC 99.
               05  QSPB-INSMAXAGE-5-4                 PIC 99.
               05  QSPB-INSMAXCOV-5-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-5-4                 PIC 9(3).
               05  QSPB-INSMINAGE-5-5                 PIC 99.
               05  QSPB-INSMAXAGE-5-5                 PIC 99.
               05  QSPB-INSMAXCOV-5-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-5-5                 PIC 9(3).
               05  QSPB-INSFEECD-5                    PIC XX.
               05  QSPB-INSFEE-FRMLA-5                PIC X.
               05  QSPB-INSCHGABL-FG-5                PIC X.
               05  QSPB-INSREJECT-FG-5                PIC X.
               05  QSPB-INSFINCHG-5                   PIC X.
               05  QSPB-INSCOM-6                      PIC 99.
               05  QSPB-INSRATECD-6                   PIC 999.
               05  QSPB-INSJFAC-6                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-6                   PIC 99V99.
               05  QSPB-INSCHGABL-6                   PIC X(2).
               05  QSPB-INSTERM-6-1                   PIC 9(3).
               05  QSPB-INSRATE-6-1                   PIC 99V9999.
               05  QSPB-INSTERM-6-2                   PIC 9(3).
               05  QSPB-INSRATE-6-2                   PIC 99V9999.
               05  QSPB-INSTERM-6-3                   PIC 9(3).
               05  QSPB-INSRATE-6-3                   PIC 99V9999.
               05  QSPB-INSTERM-6-4                   PIC 9(3).
               05  QSPB-INSRATE-6-4                   PIC 99V9999.
               05  QSPB-INSTERM-6-5                   PIC 9(3).
               05  QSPB-INSRATE-6-5                   PIC 99V9999.
               05  QSPB-INSTERM-6-6                   PIC 9(3).
               05  QSPB-INSRATE-6-6                   PIC 99V9999.
               05  QSPB-INSTERM-6-7                   PIC 9(3).
               05  QSPB-INSRATE-6-7                   PIC 99V9999.
               05  QSPB-INSTERM-6-8                   PIC 9(3).
               05  QSPB-INSRATE-6-8                   PIC 99V9999.
               05  QSPB-INSTERM-6-9                   PIC 9(3).
               05  QSPB-INSRATE-6-9                   PIC 99V9999.
               05  QSPB-INSTERM-6-10                  PIC 9(3).
               05  QSPB-INSRATE-6-10                  PIC 99V9999.
               05  QSPB-INSTERM-6-11                  PIC 9(3).
               05  QSPB-INSRATE-6-11                  PIC 99V9999.
               05  QSPB-INSTERM-6-12                  PIC 9(3).
               05  QSPB-INSRATE-6-12                  PIC 99V9999.
               05  QSPB-INSTERM-6-13                  PIC 9(3).
               05  QSPB-INSRATE-6-13                  PIC 99V9999.
               05  QSPB-INSTERM-6-14                  PIC 9(3).
               05  QSPB-INSRATE-6-14                  PIC 99V9999.
               05  QSPB-INSTERM-6-15                  PIC 9(3).
               05  QSPB-INSRATE-6-15                  PIC 99V9999.
               05  QSPB-INSTERM-6-16                  PIC 9(3).
               05  QSPB-INSRATE-6-16                  PIC 99V9999.
               05  QSPB-INSTERM-6-17                  PIC 9(3).
               05  QSPB-INSRATE-6-17                  PIC 99V9999.
               05  QSPB-INSTERM-6-18                  PIC 9(3).
               05  QSPB-INSRATE-6-18                  PIC 99V9999.
               05  QSPB-INSTERM-6-19                  PIC 9(3).
               05  QSPB-INSRATE-6-19                  PIC 99V9999.
               05  QSPB-INSTERM-6-20                  PIC 9(3).
               05  QSPB-INSRATE-6-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-6                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-6                PIC 9(6).
               05  QSPB-INSMAXCHGABL-6                PIC 9(7).
               05  QSPB-INSMAXBENIF-6                 PIC 9(7).
               05  QSPB-INSMAXTERM-6                  PIC 9(3).
               05  QSPB-INSCHGBEN-6                   PIC X.
               05  QSPB-INSEXCEPT-6                   PIC 99.
               05  QSPB-INSNOVER-6                    PIC 99.
               05  QSPB-INSMINAGE-6-1                 PIC 99.
               05  QSPB-INSMAXAGE-6-1                 PIC 99.
               05  QSPB-INSMAXCOV-6-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-6-1                 PIC 9(3).
               05  QSPB-INSMINAGE-6-2                 PIC 99.
               05  QSPB-INSMAXAGE-6-2                 PIC 99.
               05  QSPB-INSMAXCOV-6-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-6-2                 PIC 9(3).
               05  QSPB-INSMINAGE-6-3                 PIC 99.
               05  QSPB-INSMAXAGE-6-3                 PIC 99.
               05  QSPB-INSMAXCOV-6-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-6-3                 PIC 9(3).
               05  QSPB-INSMINAGE-6-4                 PIC 99.
               05  QSPB-INSMAXAGE-6-4                 PIC 99.
               05  QSPB-INSMAXCOV-6-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-6-4                 PIC 9(3).
               05  QSPB-INSMINAGE-6-5                 PIC 99.
               05  QSPB-INSMAXAGE-6-5                 PIC 99.
               05  QSPB-INSMAXCOV-6-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-6-5                 PIC 9(3).
               05  QSPB-INSFEECD-6                    PIC XX.
               05  QSPB-INSFEE-FRMLA-6                PIC X.
               05  QSPB-INSCHGABL-FG-6                PIC X.
               05  QSPB-INSREJECT-FG-6                PIC X.
               05  QSPB-INSFINCHG-6                   PIC X.
               05  QSPB-INSCOM-7                      PIC 99.
               05  QSPB-INSRATECD-7                   PIC 999.
               05  QSPB-INSJFAC-7                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-7                   PIC 99V99.
               05  QSPB-INSCHGABL-7                   PIC X(2).
               05  QSPB-INSTERM-7-1                   PIC 9(3).
               05  QSPB-INSRATE-7-1                   PIC 99V9999.
               05  QSPB-INSTERM-7-2                   PIC 9(3).
               05  QSPB-INSRATE-7-2                   PIC 99V9999.
               05  QSPB-INSTERM-7-3                   PIC 9(3).
               05  QSPB-INSRATE-7-3                   PIC 99V9999.
               05  QSPB-INSTERM-7-4                   PIC 9(3).
               05  QSPB-INSRATE-7-4                   PIC 99V9999.
               05  QSPB-INSTERM-7-5                   PIC 9(3).
               05  QSPB-INSRATE-7-5                   PIC 99V9999.
               05  QSPB-INSTERM-7-6                   PIC 9(3).
               05  QSPB-INSRATE-7-6                   PIC 99V9999.
               05  QSPB-INSTERM-7-7                   PIC 9(3).
               05  QSPB-INSRATE-7-7                   PIC 99V9999.
               05  QSPB-INSTERM-7-8                   PIC 9(3).
               05  QSPB-INSRATE-7-8                   PIC 99V9999.
               05  QSPB-INSTERM-7-9                   PIC 9(3).
               05  QSPB-INSRATE-7-9                   PIC 99V9999.
               05  QSPB-INSTERM-7-10                  PIC 9(3).
               05  QSPB-INSRATE-7-10                  PIC 99V9999.
               05  QSPB-INSTERM-7-11                  PIC 9(3).
               05  QSPB-INSRATE-7-11                  PIC 99V9999.
               05  QSPB-INSTERM-7-12                  PIC 9(3).
               05  QSPB-INSRATE-7-12                  PIC 99V9999.
               05  QSPB-INSTERM-7-13                  PIC 9(3).
               05  QSPB-INSRATE-7-13                  PIC 99V9999.
               05  QSPB-INSTERM-7-14                  PIC 9(3).
               05  QSPB-INSRATE-7-14                  PIC 99V9999.
               05  QSPB-INSTERM-7-15                  PIC 9(3).
               05  QSPB-INSRATE-7-15                  PIC 99V9999.
               05  QSPB-INSTERM-7-16                  PIC 9(3).
               05  QSPB-INSRATE-7-16                  PIC 99V9999.
               05  QSPB-INSTERM-7-17                  PIC 9(3).
               05  QSPB-INSRATE-7-17                  PIC 99V9999.
               05  QSPB-INSTERM-7-18                  PIC 9(3).
               05  QSPB-INSRATE-7-18                  PIC 99V9999.
               05  QSPB-INSTERM-7-19                  PIC 9(3).
               05  QSPB-INSRATE-7-19                  PIC 99V9999.
               05  QSPB-INSTERM-7-20                  PIC 9(3).
               05  QSPB-INSRATE-7-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-7                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-7                PIC 9(6).
               05  QSPB-INSMAXCHGABL-7                PIC 9(7).
               05  QSPB-INSMAXBENIF-7                 PIC 9(7).
               05  QSPB-INSMAXTERM-7                  PIC 9(3).
               05  QSPB-INSCHGBEN-7                   PIC X.
               05  QSPB-INSEXCEPT-7                   PIC 99.
               05  QSPB-INSNOVER-7                    PIC 99.
               05  QSPB-INSMINAGE-7-1                 PIC 99.
               05  QSPB-INSMAXAGE-7-1                 PIC 99.
               05  QSPB-INSMAXCOV-7-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-7-1                 PIC 9(3).
               05  QSPB-INSMINAGE-7-2                 PIC 99.
               05  QSPB-INSMAXAGE-7-2                 PIC 99.
               05  QSPB-INSMAXCOV-7-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-7-2                 PIC 9(3).
               05  QSPB-INSMINAGE-7-3                 PIC 99.
               05  QSPB-INSMAXAGE-7-3                 PIC 99.
               05  QSPB-INSMAXCOV-7-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-7-3                 PIC 9(3).
               05  QSPB-INSMINAGE-7-4                 PIC 99.
               05  QSPB-INSMAXAGE-7-4                 PIC 99.
               05  QSPB-INSMAXCOV-7-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-7-4                 PIC 9(3).
               05  QSPB-INSMINAGE-7-5                 PIC 99.
               05  QSPB-INSMAXAGE-7-5                 PIC 99.
               05  QSPB-INSMAXCOV-7-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-7-5                 PIC 9(3).
               05  QSPB-INSFEECD-7                    PIC XX.
               05  QSPB-INSFEE-FRMLA-7                PIC X.
               05  QSPB-INSCHGABL-FG-7                PIC X.
               05  QSPB-INSREJECT-FG-7                PIC X.
               05  QSPB-INSFINCHG-7                   PIC X.
               05  QSPB-INSCOM-8                      PIC 99.
               05  QSPB-INSRATECD-8                   PIC 999.
               05  QSPB-INSJFAC-8                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-8                   PIC 99V99.
               05  QSPB-INSCHGABL-8                   PIC X(2).
               05  QSPB-INSTERM-8-1                   PIC 9(3).
               05  QSPB-INSRATE-8-1                   PIC 99V9999.
               05  QSPB-INSTERM-8-2                   PIC 9(3).
               05  QSPB-INSRATE-8-2                   PIC 99V9999.
               05  QSPB-INSTERM-8-3                   PIC 9(3).
               05  QSPB-INSRATE-8-3                   PIC 99V9999.
               05  QSPB-INSTERM-8-4                   PIC 9(3).
               05  QSPB-INSRATE-8-4                   PIC 99V9999.
               05  QSPB-INSTERM-8-5                   PIC 9(3).
               05  QSPB-INSRATE-8-5                   PIC 99V9999.
               05  QSPB-INSTERM-8-6                   PIC 9(3).
               05  QSPB-INSRATE-8-6                   PIC 99V9999.
               05  QSPB-INSTERM-8-7                   PIC 9(3).
               05  QSPB-INSRATE-8-7                   PIC 99V9999.
               05  QSPB-INSTERM-8-8                   PIC 9(3).
               05  QSPB-INSRATE-8-8                   PIC 99V9999.
               05  QSPB-INSTERM-8-9                   PIC 9(3).
               05  QSPB-INSRATE-8-9                   PIC 99V9999.
               05  QSPB-INSTERM-8-10                  PIC 9(3).
               05  QSPB-INSRATE-8-10                  PIC 99V9999.
               05  QSPB-INSTERM-8-11                  PIC 9(3).
               05  QSPB-INSRATE-8-11                  PIC 99V9999.
               05  QSPB-INSTERM-8-12                  PIC 9(3).
               05  QSPB-INSRATE-8-12                  PIC 99V9999.
               05  QSPB-INSTERM-8-13                  PIC 9(3).
               05  QSPB-INSRATE-8-13                  PIC 99V9999.
               05  QSPB-INSTERM-8-14                  PIC 9(3).
               05  QSPB-INSRATE-8-14                  PIC 99V9999.
               05  QSPB-INSTERM-8-15                  PIC 9(3).
               05  QSPB-INSRATE-8-15                  PIC 99V9999.
               05  QSPB-INSTERM-8-16                  PIC 9(3).
               05  QSPB-INSRATE-8-16                  PIC 99V9999.
               05  QSPB-INSTERM-8-17                  PIC 9(3).
               05  QSPB-INSRATE-8-17                  PIC 99V9999.
               05  QSPB-INSTERM-8-18                  PIC 9(3).
               05  QSPB-INSRATE-8-18                  PIC 99V9999.
               05  QSPB-INSTERM-8-19                  PIC 9(3).
               05  QSPB-INSRATE-8-19                  PIC 99V9999.
               05  QSPB-INSTERM-8-20                  PIC 9(3).
               05  QSPB-INSRATE-8-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-8                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-8                PIC 9(6).
               05  QSPB-INSMAXCHGABL-8                PIC 9(7).
               05  QSPB-INSMAXBENIF-8                 PIC 9(7).
               05  QSPB-INSMAXTERM-8                  PIC 9(3).
               05  QSPB-INSCHGBEN-8                   PIC X.
               05  QSPB-INSEXCEPT-8                   PIC 99.
               05  QSPB-INSNOVER-8                    PIC 99.
               05  QSPB-INSMINAGE-8-1                 PIC 99.
               05  QSPB-INSMAXAGE-8-1                 PIC 99.
               05  QSPB-INSMAXCOV-8-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-8-1                 PIC 9(3).
               05  QSPB-INSMINAGE-8-2                 PIC 99.
               05  QSPB-INSMAXAGE-8-2                 PIC 99.
               05  QSPB-INSMAXCOV-8-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-8-2                 PIC 9(3).
               05  QSPB-INSMINAGE-8-3                 PIC 99.
               05  QSPB-INSMAXAGE-8-3                 PIC 99.
               05  QSPB-INSMAXCOV-8-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-8-3                 PIC 9(3).
               05  QSPB-INSMINAGE-8-4                 PIC 99.
               05  QSPB-INSMAXAGE-8-4                 PIC 99.
               05  QSPB-INSMAXCOV-8-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-8-4                 PIC 9(3).
               05  QSPB-INSMINAGE-8-5                 PIC 99.
               05  QSPB-INSMAXAGE-8-5                 PIC 99.
               05  QSPB-INSMAXCOV-8-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-8-5                 PIC 9(3).
               05  QSPB-INSFEECD-8                    PIC XX.
               05  QSPB-INSFEE-FRMLA-8                PIC X.
               05  QSPB-INSCHGABL-FG-8                PIC X.
               05  QSPB-INSREJECT-FG-8                PIC X.
               05  QSPB-INSFINCHG-8                   PIC X.
               05  QSPB-INSCOM-9                      PIC 99.
               05  QSPB-INSRATECD-9                   PIC 999.
               05  QSPB-INSJFAC-9                     PIC 9(4)V99.
               05  QSPB-INSDISCNT-9                   PIC 99V99.
               05  QSPB-INSCHGABL-9                   PIC X(2).
               05  QSPB-INSTERM-9-1                   PIC 9(3).
               05  QSPB-INSRATE-9-1                   PIC 99V9999.
               05  QSPB-INSTERM-9-2                   PIC 9(3).
               05  QSPB-INSRATE-9-2                   PIC 99V9999.
               05  QSPB-INSTERM-9-3                   PIC 9(3).
               05  QSPB-INSRATE-9-3                   PIC 99V9999.
               05  QSPB-INSTERM-9-4                   PIC 9(3).
               05  QSPB-INSRATE-9-4                   PIC 99V9999.
               05  QSPB-INSTERM-9-5                   PIC 9(3).
               05  QSPB-INSRATE-9-5                   PIC 99V9999.
               05  QSPB-INSTERM-9-6                   PIC 9(3).
               05  QSPB-INSRATE-9-6                   PIC 99V9999.
               05  QSPB-INSTERM-9-7                   PIC 9(3).
               05  QSPB-INSRATE-9-7                   PIC 99V9999.
               05  QSPB-INSTERM-9-8                   PIC 9(3).
               05  QSPB-INSRATE-9-8                   PIC 99V9999.
               05  QSPB-INSTERM-9-9                   PIC 9(3).
               05  QSPB-INSRATE-9-9                   PIC 99V9999.
               05  QSPB-INSTERM-9-10                  PIC 9(3).
               05  QSPB-INSRATE-9-10                  PIC 99V9999.
               05  QSPB-INSTERM-9-11                  PIC 9(3).
               05  QSPB-INSRATE-9-11                  PIC 99V9999.
               05  QSPB-INSTERM-9-12                  PIC 9(3).
               05  QSPB-INSRATE-9-12                  PIC 99V9999.
               05  QSPB-INSTERM-9-13                  PIC 9(3).
               05  QSPB-INSRATE-9-13                  PIC 99V9999.
               05  QSPB-INSTERM-9-14                  PIC 9(3).
               05  QSPB-INSRATE-9-14                  PIC 99V9999.
               05  QSPB-INSTERM-9-15                  PIC 9(3).
               05  QSPB-INSRATE-9-15                  PIC 99V9999.
               05  QSPB-INSTERM-9-16                  PIC 9(3).
               05  QSPB-INSRATE-9-16                  PIC 99V9999.
               05  QSPB-INSTERM-9-17                  PIC 9(3).
               05  QSPB-INSRATE-9-17                  PIC 99V9999.
               05  QSPB-INSTERM-9-18                  PIC 9(3).
               05  QSPB-INSRATE-9-18                  PIC 99V9999.
               05  QSPB-INSTERM-9-19                  PIC 9(3).
               05  QSPB-INSRATE-9-19                  PIC 99V9999.
               05  QSPB-INSTERM-9-20                  PIC 9(3).
               05  QSPB-INSRATE-9-20                  PIC 99V9999.
               05  QSPB-INSMINPREM-9                  PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-9                PIC 9(6).
               05  QSPB-INSMAXCHGABL-9                PIC 9(7).
               05  QSPB-INSMAXBENIF-9                 PIC 9(7).
               05  QSPB-INSMAXTERM-9                  PIC 9(3).
               05  QSPB-INSCHGBEN-9                   PIC X.
               05  QSPB-INSEXCEPT-9                   PIC 99.
               05  QSPB-INSNOVER-9                    PIC 99.
               05  QSPB-INSMINAGE-9-1                 PIC 99.
               05  QSPB-INSMAXAGE-9-1                 PIC 99.
               05  QSPB-INSMAXCOV-9-1                 PIC 9(7).
               05  QSPB-INSMAXTRM-9-1                 PIC 9(3).
               05  QSPB-INSMINAGE-9-2                 PIC 99.
               05  QSPB-INSMAXAGE-9-2                 PIC 99.
               05  QSPB-INSMAXCOV-9-2                 PIC 9(7).
               05  QSPB-INSMAXTRM-9-2                 PIC 9(3).
               05  QSPB-INSMINAGE-9-3                 PIC 99.
               05  QSPB-INSMAXAGE-9-3                 PIC 99.
               05  QSPB-INSMAXCOV-9-3                 PIC 9(7).
               05  QSPB-INSMAXTRM-9-3                 PIC 9(3).
               05  QSPB-INSMINAGE-9-4                 PIC 99.
               05  QSPB-INSMAXAGE-9-4                 PIC 99.
               05  QSPB-INSMAXCOV-9-4                 PIC 9(7).
               05  QSPB-INSMAXTRM-9-4                 PIC 9(3).
               05  QSPB-INSMINAGE-9-5                 PIC 99.
               05  QSPB-INSMAXAGE-9-5                 PIC 99.
               05  QSPB-INSMAXCOV-9-5                 PIC 9(7).
               05  QSPB-INSMAXTRM-9-5                 PIC 9(3).
               05  QSPB-INSFEECD-9                    PIC XX.
               05  QSPB-INSFEE-FRMLA-9                PIC X.
               05  QSPB-INSCHGABL-FG-9                PIC X.
               05  QSPB-INSREJECT-FG-9                PIC X.
               05  QSPB-INSFINCHG-9                   PIC X.
               05  QSPB-INSCOM-10                     PIC 99.
               05  QSPB-INSRATECD-10                  PIC 999.
               05  QSPB-INSJFAC-10                    PIC 9(4)V99.
               05  QSPB-INSDISCNT-10                  PIC 99V99.
               05  QSPB-INSCHGABL-10                  PIC X(2).
               05  QSPB-INSTERM-10-1                  PIC 9(3).
               05  QSPB-INSRATE-10-1                  PIC 99V9999.
               05  QSPB-INSTERM-10-2                  PIC 9(3).
               05  QSPB-INSRATE-10-2                  PIC 99V9999.
               05  QSPB-INSTERM-10-3                  PIC 9(3).
               05  QSPB-INSRATE-10-3                  PIC 99V9999.
               05  QSPB-INSTERM-10-4                  PIC 9(3).
               05  QSPB-INSRATE-10-4                  PIC 99V9999.
               05  QSPB-INSTERM-10-5                  PIC 9(3).
               05  QSPB-INSRATE-10-5                  PIC 99V9999.
               05  QSPB-INSTERM-10-6                  PIC 9(3).
               05  QSPB-INSRATE-10-6                  PIC 99V9999.
               05  QSPB-INSTERM-10-7                  PIC 9(3).
               05  QSPB-INSRATE-10-7                  PIC 99V9999.
               05  QSPB-INSTERM-10-8                  PIC 9(3).
               05  QSPB-INSRATE-10-8                  PIC 99V9999.
               05  QSPB-INSTERM-10-9                  PIC 9(3).
               05  QSPB-INSRATE-10-9                  PIC 99V9999.
               05  QSPB-INSTERM-10-10                 PIC 9(3).
               05  QSPB-INSRATE-10-10                 PIC 99V9999.
               05  QSPB-INSTERM-10-11                 PIC 9(3).
               05  QSPB-INSRATE-10-11                 PIC 99V9999.
               05  QSPB-INSTERM-10-12                 PIC 9(3).
               05  QSPB-INSRATE-10-12                 PIC 99V9999.
               05  QSPB-INSTERM-10-13                 PIC 9(3).
               05  QSPB-INSRATE-10-13                 PIC 99V9999.
               05  QSPB-INSTERM-10-14                 PIC 9(3).
               05  QSPB-INSRATE-10-14                 PIC 99V9999.
               05  QSPB-INSTERM-10-15                 PIC 9(3).
               05  QSPB-INSRATE-10-15                 PIC 99V9999.
               05  QSPB-INSTERM-10-16                 PIC 9(3).
               05  QSPB-INSRATE-10-16                 PIC 99V9999.
               05  QSPB-INSTERM-10-17                 PIC 9(3).
               05  QSPB-INSRATE-10-17                 PIC 99V9999.
               05  QSPB-INSTERM-10-18                 PIC 9(3).
               05  QSPB-INSRATE-10-18                 PIC 99V9999.
               05  QSPB-INSTERM-10-19                 PIC 9(3).
               05  QSPB-INSRATE-10-19                 PIC 99V9999.
               05  QSPB-INSTERM-10-20                 PIC 9(3).
               05  QSPB-INSRATE-10-20                 PIC 99V9999.
               05  QSPB-INSMINPREM-10                 PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-10               PIC 9(6).
               05  QSPB-INSMAXCHGABL-10               PIC 9(7).
               05  QSPB-INSMAXBENIF-10                PIC 9(7).
               05  QSPB-INSMAXTERM-10                 PIC 9(3).
               05  QSPB-INSCHGBEN-10                  PIC X.
               05  QSPB-INSEXCEPT-10                  PIC 99.
               05  QSPB-INSNOVER-10                   PIC 99.
               05  QSPB-INSMINAGE-10-1                PIC 99.
               05  QSPB-INSMAXAGE-10-1                PIC 99.
               05  QSPB-INSMAXCOV-10-1                PIC 9(7).
               05  QSPB-INSMAXTRM-10-1                PIC 9(3).
               05  QSPB-INSMINAGE-10-2                PIC 99.
               05  QSPB-INSMAXAGE-10-2                PIC 99.
               05  QSPB-INSMAXCOV-10-2                PIC 9(7).
               05  QSPB-INSMAXTRM-10-2                PIC 9(3).
               05  QSPB-INSMINAGE-10-3                PIC 99.
               05  QSPB-INSMAXAGE-10-3                PIC 99.
               05  QSPB-INSMAXCOV-10-3                PIC 9(7).
               05  QSPB-INSMAXTRM-10-3                PIC 9(3).
               05  QSPB-INSMINAGE-10-4                PIC 99.
               05  QSPB-INSMAXAGE-10-4                PIC 99.
               05  QSPB-INSMAXCOV-10-4                PIC 9(7).
               05  QSPB-INSMAXTRM-10-4                PIC 9(3).
               05  QSPB-INSMINAGE-10-5                PIC 99.
               05  QSPB-INSMAXAGE-10-5                PIC 99.
               05  QSPB-INSMAXCOV-10-5                PIC 9(7).
               05  QSPB-INSMAXTRM-10-5                PIC 9(3).
               05  QSPB-INSFEECD-10                   PIC XX.
               05  QSPB-INSFEE-FRMLA-10               PIC X.
               05  QSPB-INSCHGABL-FG-10               PIC X.
               05  QSPB-INSREJECT-FG-10               PIC X.
               05  QSPB-INSFINCHG-10                  PIC X.
               05  QSPB-INSCOM-11                     PIC 99.
               05  QSPB-INSRATECD-11                  PIC 999.
               05  QSPB-INSJFAC-11                    PIC 9(4)V99.
               05  QSPB-INSDISCNT-11                  PIC 99V99.
               05  QSPB-INSCHGABL-11                  PIC X(2).
               05  QSPB-INSTERM-11-1                  PIC 9(3).
               05  QSPB-INSRATE-11-1                  PIC 99V9999.
               05  QSPB-INSTERM-11-2                  PIC 9(3).
               05  QSPB-INSRATE-11-2                  PIC 99V9999.
               05  QSPB-INSTERM-11-3                  PIC 9(3).
               05  QSPB-INSRATE-11-3                  PIC 99V9999.
               05  QSPB-INSTERM-11-4                  PIC 9(3).
               05  QSPB-INSRATE-11-4                  PIC 99V9999.
               05  QSPB-INSTERM-11-5                  PIC 9(3).
               05  QSPB-INSRATE-11-5                  PIC 99V9999.
               05  QSPB-INSTERM-11-6                  PIC 9(3).
               05  QSPB-INSRATE-11-6                  PIC 99V9999.
               05  QSPB-INSTERM-11-7                  PIC 9(3).
               05  QSPB-INSRATE-11-7                  PIC 99V9999.
               05  QSPB-INSTERM-11-8                  PIC 9(3).
               05  QSPB-INSRATE-11-8                  PIC 99V9999.
               05  QSPB-INSTERM-11-9                  PIC 9(3).
               05  QSPB-INSRATE-11-9                  PIC 99V9999.
               05  QSPB-INSTERM-11-10                 PIC 9(3).
               05  QSPB-INSRATE-11-10                 PIC 99V9999.
               05  QSPB-INSTERM-11-11                 PIC 9(3).
               05  QSPB-INSRATE-11-11                 PIC 99V9999.
               05  QSPB-INSTERM-11-12                 PIC 9(3).
               05  QSPB-INSRATE-11-12                 PIC 99V9999.
               05  QSPB-INSTERM-11-13                 PIC 9(3).
               05  QSPB-INSRATE-11-13                 PIC 99V9999.
               05  QSPB-INSTERM-11-14                 PIC 9(3).
               05  QSPB-INSRATE-11-14                 PIC 99V9999.
               05  QSPB-INSTERM-11-15                 PIC 9(3).
               05  QSPB-INSRATE-11-15                 PIC 99V9999.
               05  QSPB-INSTERM-11-16                 PIC 9(3).
               05  QSPB-INSRATE-11-16                 PIC 99V9999.
               05  QSPB-INSTERM-11-17                 PIC 9(3).
               05  QSPB-INSRATE-11-17                 PIC 99V9999.
               05  QSPB-INSTERM-11-18                 PIC 9(3).
               05  QSPB-INSRATE-11-18                 PIC 99V9999.
               05  QSPB-INSTERM-11-19                 PIC 9(3).
               05  QSPB-INSRATE-11-19                 PIC 99V9999.
               05  QSPB-INSTERM-11-20                 PIC 9(3).
               05  QSPB-INSRATE-11-20                 PIC 99V9999.
               05  QSPB-INSMINPREM-11                 PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-11               PIC 9(6).
               05  QSPB-INSMAXCHGABL-11               PIC 9(7).
               05  QSPB-INSMAXBENIF-11                PIC 9(7).
               05  QSPB-INSMAXTERM-11                 PIC 9(3).
               05  QSPB-INSCHGBEN-11                  PIC X.
               05  QSPB-INSEXCEPT-11                  PIC 99.
               05  QSPB-INSNOVER-11                   PIC 99.
               05  QSPB-INSMINAGE-11-1                PIC 99.
               05  QSPB-INSMAXAGE-11-1                PIC 99.
               05  QSPB-INSMAXCOV-11-1                PIC 9(7).
               05  QSPB-INSMAXTRM-11-1                PIC 9(3).
               05  QSPB-INSMINAGE-11-2                PIC 99.
               05  QSPB-INSMAXAGE-11-2                PIC 99.
               05  QSPB-INSMAXCOV-11-2                PIC 9(7).
               05  QSPB-INSMAXTRM-11-2                PIC 9(3).
               05  QSPB-INSMINAGE-11-3                PIC 99.
               05  QSPB-INSMAXAGE-11-3                PIC 99.
               05  QSPB-INSMAXCOV-11-3                PIC 9(7).
               05  QSPB-INSMAXTRM-11-3                PIC 9(3).
               05  QSPB-INSMINAGE-11-4                PIC 99.
               05  QSPB-INSMAXAGE-11-4                PIC 99.
               05  QSPB-INSMAXCOV-11-4                PIC 9(7).
               05  QSPB-INSMAXTRM-11-4                PIC 9(3).
               05  QSPB-INSMINAGE-11-5                PIC 99.
               05  QSPB-INSMAXAGE-11-5                PIC 99.
               05  QSPB-INSMAXCOV-11-5                PIC 9(7).
               05  QSPB-INSMAXTRM-11-5                PIC 9(3).
               05  QSPB-INSFEECD-11                   PIC XX.
               05  QSPB-INSFEE-FRMLA-11               PIC X.
               05  QSPB-INSCHGABL-FG-11               PIC X.
               05  QSPB-INSREJECT-FG-11               PIC X.
               05  QSPB-INSFINCHG-11                  PIC X.
               05  QSPB-INSCOM-12                     PIC 99.
               05  QSPB-INSRATECD-12                  PIC 999.
               05  QSPB-INSJFAC-12                    PIC 9(4)V99.
               05  QSPB-INSDISCNT-12                  PIC 99V99.
               05  QSPB-INSCHGABL-12                  PIC X(2).
               05  QSPB-INSTERM-12-1                  PIC 9(3).
               05  QSPB-INSRATE-12-1                  PIC 99V9999.
               05  QSPB-INSTERM-12-2                  PIC 9(3).
               05  QSPB-INSRATE-12-2                  PIC 99V9999.
               05  QSPB-INSTERM-12-3                  PIC 9(3).
               05  QSPB-INSRATE-12-3                  PIC 99V9999.
               05  QSPB-INSTERM-12-4                  PIC 9(3).
               05  QSPB-INSRATE-12-4                  PIC 99V9999.
               05  QSPB-INSTERM-12-5                  PIC 9(3).
               05  QSPB-INSRATE-12-5                  PIC 99V9999.
               05  QSPB-INSTERM-12-6                  PIC 9(3).
               05  QSPB-INSRATE-12-6                  PIC 99V9999.
               05  QSPB-INSTERM-12-7                  PIC 9(3).
               05  QSPB-INSRATE-12-7                  PIC 99V9999.
               05  QSPB-INSTERM-12-8                  PIC 9(3).
               05  QSPB-INSRATE-12-8                  PIC 99V9999.
               05  QSPB-INSTERM-12-9                  PIC 9(3).
               05  QSPB-INSRATE-12-9                  PIC 99V9999.
               05  QSPB-INSTERM-12-10                 PIC 9(3).
               05  QSPB-INSRATE-12-10                 PIC 99V9999.
               05  QSPB-INSTERM-12-11                 PIC 9(3).
               05  QSPB-INSRATE-12-11                 PIC 99V9999.
               05  QSPB-INSTERM-12-12                 PIC 9(3).
               05  QSPB-INSRATE-12-12                 PIC 99V9999.
               05  QSPB-INSTERM-12-13                 PIC 9(3).
               05  QSPB-INSRATE-12-13                 PIC 99V9999.
               05  QSPB-INSTERM-12-14                 PIC 9(3).
               05  QSPB-INSRATE-12-14                 PIC 99V9999.
               05  QSPB-INSTERM-12-15                 PIC 9(3).
               05  QSPB-INSRATE-12-15                 PIC 99V9999.
               05  QSPB-INSTERM-12-16                 PIC 9(3).
               05  QSPB-INSRATE-12-16                 PIC 99V9999.
               05  QSPB-INSTERM-12-17                 PIC 9(3).
               05  QSPB-INSRATE-12-17                 PIC 99V9999.
               05  QSPB-INSTERM-12-18                 PIC 9(3).
               05  QSPB-INSRATE-12-18                 PIC 99V9999.
               05  QSPB-INSTERM-12-19                 PIC 9(3).
               05  QSPB-INSRATE-12-19                 PIC 99V9999.
               05  QSPB-INSTERM-12-20                 PIC 9(3).
               05  QSPB-INSRATE-12-20                 PIC 99V9999.
               05  QSPB-INSMINPREM-12                 PIC 9(4)V99.
               05  QSPB-INSMINCHGABL-12               PIC 9(6).
               05  QSPB-INSMAXCHGABL-12               PIC 9(7).
               05  QSPB-INSMAXBENIF-12                PIC 9(7).
               05  QSPB-INSMAXTERM-12                 PIC 9(3).
               05  QSPB-INSCHGBEN-12                  PIC X.
               05  QSPB-INSEXCEPT-12                  PIC 99.
               05  QSPB-INSNOVER-12                   PIC 99.
               05  QSPB-INSMINAGE-12-1                PIC 99.
               05  QSPB-INSMAXAGE-12-1                PIC 99.
               05  QSPB-INSMAXCOV-12-1                PIC 9(7).
               05  QSPB-INSMAXTRM-12-1                PIC 9(3).
               05  QSPB-INSMINAGE-12-2                PIC 99.
               05  QSPB-INSMAXAGE-12-2                PIC 99.
               05  QSPB-INSMAXCOV-12-2                PIC 9(7).
               05  QSPB-INSMAXTRM-12-2                PIC 9(3).
               05  QSPB-INSMINAGE-12-3                PIC 99.
               05  QSPB-INSMAXAGE-12-3                PIC 99.
               05  QSPB-INSMAXCOV-12-3                PIC 9(7).
               05  QSPB-INSMAXTRM-12-3                PIC 9(3).
               05  QSPB-INSMINAGE-12-4                PIC 99.
               05  QSPB-INSMAXAGE-12-4                PIC 99.
               05  QSPB-INSMAXCOV-12-4                PIC 9(7).
               05  QSPB-INSMAXTRM-12-4                PIC 9(3).
               05  QSPB-INSMINAGE-12-5                PIC 99.
               05  QSPB-INSMAXAGE-12-5                PIC 99.
               05  QSPB-INSMAXCOV-12-5                PIC 9(7).
               05  QSPB-INSMAXTRM-12-5                PIC 9(3).
               05  QSPB-INSFEECD-12                   PIC XX.
               05  QSPB-INSFEE-FRMLA-12               PIC X.
               05  QSPB-INSCHGABL-FG-12               PIC X.
               05  QSPB-INSREJECT-FG-12               PIC X.
               05  QSPB-INSFINCHG-12                  PIC X.
           03  QSPC-REC.
               05  QSPC-ORGST                         PIC XX.
               05  QSPC-SPRCLASS                      PIC 999.
               05  QSPC-SUBCLASS                      PIC 99.
               05  QSPC-LAWCODE                       PIC 99.
               05  QSPC-INSEARN-ORDER-1               PIC 9(2).
               05  QSPC-INSEARN-ORDER-2               PIC 9(2).
               05  QSPC-INSEARN-ORDER-3               PIC 9(2).
               05  QSPC-INSEARN-ORDER-4               PIC 9(2).
               05  QSPC-INSEARN-ORDER-5               PIC 9(2).
               05  QSPC-INSEARN-ORDER-6               PIC 9(2).
               05  QSPC-INSEARN-ORDER-7               PIC 9(2).
               05  QSPC-INSEARN-ORDER-8               PIC 9(2).
               05  QSPC-INSEARN-ORDER-9               PIC 9(2).
               05  QSPC-INSEARN-ORDER-10              PIC 9(2).
               05  QSPC-INSEARN-ORDER-11              PIC 9(2).
               05  QSPC-INSEARN-ORDER-12              PIC 9(2).
               05  QSPC-INSAH-JOINT-FG                PIC X.
               05  QSPC-INS-O1-EDIT                   PIC X.
               05  QSPC-INS-O2-EDIT                   PIC X.
               05  QSPC-RBFRMLA-1                     PIC X(2).
               05  QSPC-RBDTE-1-1                     PIC X.
               05  QSPC-RBDAYS-1-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-1-1                  PIC 9(4).
               05  QSPC-RBDTE-1-2                     PIC X.
               05  QSPC-RBDAYS-1-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-1-2                  PIC 9(4).
               05  QSPC-RBMINRET-1                    PIC 9999V99.
               05  QSPC-RBNOMON-1                     PIC 9(2).
               05  QSPC-RBDEDCHG-1                    PIC 9999V99.
               05  QSPC-RBRECAST-1                    PIC 9(2).
               05  QSPC-RBEARLY-1                     PIC 9(2).
               05  QSPC-RBDEFER-1                     PIC 9(2).
               05  QSPC-RBPCT-1                       PIC 9(3)V9.
               05  QSPC-RBREDUC-1                     PIC 9(4)V99.
               05  QSPC-RBMIN-1                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-1                    PIC 9(2).
               05  QSPC-RBSPOPT2-1                    PIC 9(2).
               05  QSPC-RBMANAGER-1                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-1                PIC X.
               05  QSPC-RBFRMLA2-1                    PIC X(2).
               05  QSPC-RBFRMLA-2                     PIC X(2).
               05  QSPC-RBDTE-2-1                     PIC X.
               05  QSPC-RBDAYS-2-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-2-1                  PIC 9(4).
               05  QSPC-RBDTE-2-2                     PIC X.
               05  QSPC-RBDAYS-2-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-2-2                  PIC 9(4).
               05  QSPC-RBMINRET-2                    PIC 9999V99.
               05  QSPC-RBNOMON-2                     PIC 9(2).
               05  QSPC-RBDEDCHG-2                    PIC 9999V99.
               05  QSPC-RBRECAST-2                    PIC 9(2).
               05  QSPC-RBEARLY-2                     PIC 9(2).
               05  QSPC-RBDEFER-2                     PIC 9(2).
               05  QSPC-RBPCT-2                       PIC 9(3)V9.
               05  QSPC-RBREDUC-2                     PIC 9(4)V99.
               05  QSPC-RBMIN-2                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-2                    PIC 9(2).
               05  QSPC-RBSPOPT2-2                    PIC 9(2).
               05  QSPC-RBMANAGER-2                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-2                PIC X.
               05  QSPC-RBFRMLA2-2                    PIC X(2).
               05  QSPC-RBFRMLA-3                     PIC X(2).
               05  QSPC-RBDTE-3-1                     PIC X.
               05  QSPC-RBDAYS-3-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-3-1                  PIC 9(4).
               05  QSPC-RBDTE-3-2                     PIC X.
               05  QSPC-RBDAYS-3-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-3-2                  PIC 9(4).
               05  QSPC-RBMINRET-3                    PIC 9999V99.
               05  QSPC-RBNOMON-3                     PIC 9(2).
               05  QSPC-RBDEDCHG-3                    PIC 9999V99.
               05  QSPC-RBRECAST-3                    PIC 9(2).
               05  QSPC-RBEARLY-3                     PIC 9(2).
               05  QSPC-RBDEFER-3                     PIC 9(2).
               05  QSPC-RBPCT-3                       PIC 9(3)V9.
               05  QSPC-RBREDUC-3                     PIC 9(4)V99.
               05  QSPC-RBMIN-3                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-3                    PIC 9(2).
               05  QSPC-RBSPOPT2-3                    PIC 9(2).
               05  QSPC-RBMANAGER-3                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-3                PIC X.
               05  QSPC-RBFRMLA2-3                    PIC X(2).
               05  QSPC-RBFRMLA-4                     PIC X(2).
               05  QSPC-RBDTE-4-1                     PIC X.
               05  QSPC-RBDAYS-4-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-4-1                  PIC 9(4).
               05  QSPC-RBDTE-4-2                     PIC X.
               05  QSPC-RBDAYS-4-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-4-2                  PIC 9(4).
               05  QSPC-RBMINRET-4                    PIC 9999V99.
               05  QSPC-RBNOMON-4                     PIC 9(2).
               05  QSPC-RBDEDCHG-4                    PIC 9999V99.
               05  QSPC-RBRECAST-4                    PIC 9(2).
               05  QSPC-RBEARLY-4                     PIC 9(2).
               05  QSPC-RBDEFER-4                     PIC 9(2).
               05  QSPC-RBPCT-4                       PIC 9(3)V9.
               05  QSPC-RBREDUC-4                     PIC 9(4)V99.
               05  QSPC-RBMIN-4                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-4                    PIC 9(2).
               05  QSPC-RBSPOPT2-4                    PIC 9(2).
               05  QSPC-RBMANAGER-4                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-4                PIC X.
               05  QSPC-RBFRMLA2-4                    PIC X(2).
               05  QSPC-RBFRMLA-5                     PIC X(2).
               05  QSPC-RBDTE-5-1                     PIC X.
               05  QSPC-RBDAYS-5-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-5-1                  PIC 9(4).
               05  QSPC-RBDTE-5-2                     PIC X.
               05  QSPC-RBDAYS-5-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-5-2                  PIC 9(4).
               05  QSPC-RBMINRET-5                    PIC 9999V99.
               05  QSPC-RBNOMON-5                     PIC 9(2).
               05  QSPC-RBDEDCHG-5                    PIC 9999V99.
               05  QSPC-RBRECAST-5                    PIC 9(2).
               05  QSPC-RBEARLY-5                     PIC 9(2).
               05  QSPC-RBDEFER-5                     PIC 9(2).
               05  QSPC-RBPCT-5                       PIC 9(3)V9.
               05  QSPC-RBREDUC-5                     PIC 9(4)V99.
               05  QSPC-RBMIN-5                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-5                    PIC 9(2).
               05  QSPC-RBSPOPT2-5                    PIC 9(2).
               05  QSPC-RBMANAGER-5                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-5                PIC X.
               05  QSPC-RBFRMLA2-5                    PIC X(2).
               05  QSPC-RBFRMLA-6                     PIC X(2).
               05  QSPC-RBDTE-6-1                     PIC X.
               05  QSPC-RBDAYS-6-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-6-1                  PIC 9(4).
               05  QSPC-RBDTE-6-2                     PIC X.
               05  QSPC-RBDAYS-6-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-6-2                  PIC 9(4).
               05  QSPC-RBMINRET-6                    PIC 9999V99.
               05  QSPC-RBNOMON-6                     PIC 9(2).
               05  QSPC-RBDEDCHG-6                    PIC 9999V99.
               05  QSPC-RBRECAST-6                    PIC 9(2).
               05  QSPC-RBEARLY-6                     PIC 9(2).
               05  QSPC-RBDEFER-6                     PIC 9(2).
               05  QSPC-RBPCT-6                       PIC 9(3)V9.
               05  QSPC-RBREDUC-6                     PIC 9(4)V99.
               05  QSPC-RBMIN-6                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-6                    PIC 9(2).
               05  QSPC-RBSPOPT2-6                    PIC 9(2).
               05  QSPC-RBMANAGER-6                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-6                PIC X.
               05  QSPC-RBFRMLA2-6                    PIC X(2).
               05  QSPC-RBFRMLA-7                     PIC X(2).
               05  QSPC-RBDTE-7-1                     PIC X.
               05  QSPC-RBDAYS-7-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-7-1                  PIC 9(4).
               05  QSPC-RBDTE-7-2                     PIC X.
               05  QSPC-RBDAYS-7-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-7-2                  PIC 9(4).
               05  QSPC-RBMINRET-7                    PIC 9999V99.
               05  QSPC-RBNOMON-7                     PIC 9(2).
               05  QSPC-RBDEDCHG-7                    PIC 9999V99.
               05  QSPC-RBRECAST-7                    PIC 9(2).
               05  QSPC-RBEARLY-7                     PIC 9(2).
               05  QSPC-RBDEFER-7                     PIC 9(2).
               05  QSPC-RBPCT-7                       PIC 9(3)V9.
               05  QSPC-RBREDUC-7                     PIC 9(4)V99.
               05  QSPC-RBMIN-7                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-7                    PIC 9(2).
               05  QSPC-RBSPOPT2-7                    PIC 9(2).
               05  QSPC-RBMANAGER-7                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-7                PIC X.
               05  QSPC-RBFRMLA2-7                    PIC X(2).
               05  QSPC-RBFRMLA-8                     PIC X(2).
               05  QSPC-RBDTE-8-1                     PIC X.
               05  QSPC-RBDAYS-8-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-8-1                  PIC 9(4).
               05  QSPC-RBDTE-8-2                     PIC X.
               05  QSPC-RBDAYS-8-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-8-2                  PIC 9(4).
               05  QSPC-RBMINRET-8                    PIC 9999V99.
               05  QSPC-RBNOMON-8                     PIC 9(2).
               05  QSPC-RBDEDCHG-8                    PIC 9999V99.
               05  QSPC-RBRECAST-8                    PIC 9(2).
               05  QSPC-RBEARLY-8                     PIC 9(2).
               05  QSPC-RBDEFER-8                     PIC 9(2).
               05  QSPC-RBPCT-8                       PIC 9(3)V9.
               05  QSPC-RBREDUC-8                     PIC 9(4)V99.
               05  QSPC-RBMIN-8                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-8                    PIC 9(2).
               05  QSPC-RBSPOPT2-8                    PIC 9(2).
               05  QSPC-RBMANAGER-8                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-8                PIC X.
               05  QSPC-RBFRMLA2-8                    PIC X(2).
               05  QSPC-RBFRMLA-9                     PIC X(2).
               05  QSPC-RBDTE-9-1                     PIC X.
               05  QSPC-RBDAYS-9-1                    PIC 9(2).
               05  QSPC-RBYRTYPE-9-1                  PIC 9(4).
               05  QSPC-RBDTE-9-2                     PIC X.
               05  QSPC-RBDAYS-9-2                    PIC 9(2).
               05  QSPC-RBYRTYPE-9-2                  PIC 9(4).
               05  QSPC-RBMINRET-9                    PIC 9999V99.
               05  QSPC-RBNOMON-9                     PIC 9(2).
               05  QSPC-RBDEDCHG-9                    PIC 9999V99.
               05  QSPC-RBRECAST-9                    PIC 9(2).
               05  QSPC-RBEARLY-9                     PIC 9(2).
               05  QSPC-RBDEFER-9                     PIC 9(2).
               05  QSPC-RBPCT-9                       PIC 9(3)V9.
               05  QSPC-RBREDUC-9                     PIC 9(4)V99.
               05  QSPC-RBMIN-9                       PIC 9(4)V99.
               05  QSPC-RBSPOPT1-9                    PIC 9(2).
               05  QSPC-RBSPOPT2-9                    PIC 9(2).
               05  QSPC-RBMANAGER-9                   PIC 9(2).
               05  QSPC-RBUNITPER-CD-9                PIC X.
               05  QSPC-RBFRMLA2-9                    PIC X(2).
               05  QSPC-RBFRMLA-10                    PIC X(2).
               05  QSPC-RBDTE-10-1                    PIC X.
               05  QSPC-RBDAYS-10-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-10-1                 PIC 9(4).
               05  QSPC-RBDTE-10-2                    PIC X.
               05  QSPC-RBDAYS-10-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-10-2                 PIC 9(4).
               05  QSPC-RBMINRET-10                   PIC 9999V99.
               05  QSPC-RBNOMON-10                    PIC 9(2).
               05  QSPC-RBDEDCHG-10                   PIC 9999V99.
               05  QSPC-RBRECAST-10                   PIC 9(2).
               05  QSPC-RBEARLY-10                    PIC 9(2).
               05  QSPC-RBDEFER-10                    PIC 9(2).
               05  QSPC-RBPCT-10                      PIC 9(3)V9.
               05  QSPC-RBREDUC-10                    PIC 9(4)V99.
               05  QSPC-RBMIN-10                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-10                   PIC 9(2).
               05  QSPC-RBSPOPT2-10                   PIC 9(2).
               05  QSPC-RBMANAGER-10                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-10               PIC X.
               05  QSPC-RBFRMLA2-10                   PIC X(2).
               05  QSPC-RBFRMLA-11                    PIC X(2).
               05  QSPC-RBDTE-11-1                    PIC X.
               05  QSPC-RBDAYS-11-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-11-1                 PIC 9(4).
               05  QSPC-RBDTE-11-2                    PIC X.
               05  QSPC-RBDAYS-11-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-11-2                 PIC 9(4).
               05  QSPC-RBMINRET-11                   PIC 9999V99.
               05  QSPC-RBNOMON-11                    PIC 9(2).
               05  QSPC-RBDEDCHG-11                   PIC 9999V99.
               05  QSPC-RBRECAST-11                   PIC 9(2).
               05  QSPC-RBEARLY-11                    PIC 9(2).
               05  QSPC-RBDEFER-11                    PIC 9(2).
               05  QSPC-RBPCT-11                      PIC 9(3)V9.
               05  QSPC-RBREDUC-11                    PIC 9(4)V99.
               05  QSPC-RBMIN-11                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-11                   PIC 9(2).
               05  QSPC-RBSPOPT2-11                   PIC 9(2).
               05  QSPC-RBMANAGER-11                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-11               PIC X.
               05  QSPC-RBFRMLA2-11                   PIC X(2).
               05  QSPC-RBFRMLA-12                    PIC X(2).
               05  QSPC-RBDTE-12-1                    PIC X.
               05  QSPC-RBDAYS-12-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-12-1                 PIC 9(4).
               05  QSPC-RBDTE-12-2                    PIC X.
               05  QSPC-RBDAYS-12-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-12-2                 PIC 9(4).
               05  QSPC-RBMINRET-12                   PIC 9999V99.
               05  QSPC-RBNOMON-12                    PIC 9(2).
               05  QSPC-RBDEDCHG-12                   PIC 9999V99.
               05  QSPC-RBRECAST-12                   PIC 9(2).
               05  QSPC-RBEARLY-12                    PIC 9(2).
               05  QSPC-RBDEFER-12                    PIC 9(2).
               05  QSPC-RBPCT-12                      PIC 9(3)V9.
               05  QSPC-RBREDUC-12                    PIC 9(4)V99.
               05  QSPC-RBMIN-12                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-12                   PIC 9(2).
               05  QSPC-RBSPOPT2-12                   PIC 9(2).
               05  QSPC-RBMANAGER-12                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-12               PIC X.
               05  QSPC-RBFRMLA2-12                   PIC X(2).
               05  QSPC-RBFRMLA-13                    PIC X(2).
               05  QSPC-RBDTE-13-1                    PIC X.
               05  QSPC-RBDAYS-13-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-13-1                 PIC 9(4).
               05  QSPC-RBDTE-13-2                    PIC X.
               05  QSPC-RBDAYS-13-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-13-2                 PIC 9(4).
               05  QSPC-RBMINRET-13                   PIC 9999V99.
               05  QSPC-RBNOMON-13                    PIC 9(2).
               05  QSPC-RBDEDCHG-13                   PIC 9999V99.
               05  QSPC-RBRECAST-13                   PIC 9(2).
               05  QSPC-RBEARLY-13                    PIC 9(2).
               05  QSPC-RBDEFER-13                    PIC 9(2).
               05  QSPC-RBPCT-13                      PIC 9(3)V9.
               05  QSPC-RBREDUC-13                    PIC 9(4)V99.
               05  QSPC-RBMIN-13                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-13                   PIC 9(2).
               05  QSPC-RBSPOPT2-13                   PIC 9(2).
               05  QSPC-RBMANAGER-13                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-13               PIC X.
               05  QSPC-RBFRMLA2-13                   PIC X(2).
               05  QSPC-RBFRMLA-14                    PIC X(2).
               05  QSPC-RBDTE-14-1                    PIC X.
               05  QSPC-RBDAYS-14-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-14-1                 PIC 9(4).
               05  QSPC-RBDTE-14-2                    PIC X.
               05  QSPC-RBDAYS-14-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-14-2                 PIC 9(4).
               05  QSPC-RBMINRET-14                   PIC 9999V99.
               05  QSPC-RBNOMON-14                    PIC 9(2).
               05  QSPC-RBDEDCHG-14                   PIC 9999V99.
               05  QSPC-RBRECAST-14                   PIC 9(2).
               05  QSPC-RBEARLY-14                    PIC 9(2).
               05  QSPC-RBDEFER-14                    PIC 9(2).
               05  QSPC-RBPCT-14                      PIC 9(3)V9.
               05  QSPC-RBREDUC-14                    PIC 9(4)V99.
               05  QSPC-RBMIN-14                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-14                   PIC 9(2).
               05  QSPC-RBSPOPT2-14                   PIC 9(2).
               05  QSPC-RBMANAGER-14                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-14               PIC X.
               05  QSPC-RBFRMLA2-14                   PIC X(2).
               05  QSPC-RBFRMLA-15                    PIC X(2).
               05  QSPC-RBDTE-15-1                    PIC X.
               05  QSPC-RBDAYS-15-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-15-1                 PIC 9(4).
               05  QSPC-RBDTE-15-2                    PIC X.
               05  QSPC-RBDAYS-15-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-15-2                 PIC 9(4).
               05  QSPC-RBMINRET-15                   PIC 9999V99.
               05  QSPC-RBNOMON-15                    PIC 9(2).
               05  QSPC-RBDEDCHG-15                   PIC 9999V99.
               05  QSPC-RBRECAST-15                   PIC 9(2).
               05  QSPC-RBEARLY-15                    PIC 9(2).
               05  QSPC-RBDEFER-15                    PIC 9(2).
               05  QSPC-RBPCT-15                      PIC 9(3)V9.
               05  QSPC-RBREDUC-15                    PIC 9(4)V99.
               05  QSPC-RBMIN-15                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-15                   PIC 9(2).
               05  QSPC-RBSPOPT2-15                   PIC 9(2).
               05  QSPC-RBMANAGER-15                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-15               PIC X.
               05  QSPC-RBFRMLA2-15                   PIC X(2).
               05  QSPC-RBFRMLA-16                    PIC X(2).
               05  QSPC-RBDTE-16-1                    PIC X.
               05  QSPC-RBDAYS-16-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-16-1                 PIC 9(4).
               05  QSPC-RBDTE-16-2                    PIC X.
               05  QSPC-RBDAYS-16-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-16-2                 PIC 9(4).
               05  QSPC-RBMINRET-16                   PIC 9999V99.
               05  QSPC-RBNOMON-16                    PIC 9(2).
               05  QSPC-RBDEDCHG-16                   PIC 9999V99.
               05  QSPC-RBRECAST-16                   PIC 9(2).
               05  QSPC-RBEARLY-16                    PIC 9(2).
               05  QSPC-RBDEFER-16                    PIC 9(2).
               05  QSPC-RBPCT-16                      PIC 9(3)V9.
               05  QSPC-RBREDUC-16                    PIC 9(4)V99.
               05  QSPC-RBMIN-16                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-16                   PIC 9(2).
               05  QSPC-RBSPOPT2-16                   PIC 9(2).
               05  QSPC-RBMANAGER-16                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-16               PIC X.
               05  QSPC-RBFRMLA2-16                   PIC X(2).
               05  QSPC-RBFRMLA-17                    PIC X(2).
               05  QSPC-RBDTE-17-1                    PIC X.
               05  QSPC-RBDAYS-17-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-17-1                 PIC 9(4).
               05  QSPC-RBDTE-17-2                    PIC X.
               05  QSPC-RBDAYS-17-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-17-2                 PIC 9(4).
               05  QSPC-RBMINRET-17                   PIC 9999V99.
               05  QSPC-RBNOMON-17                    PIC 9(2).
               05  QSPC-RBDEDCHG-17                   PIC 9999V99.
               05  QSPC-RBRECAST-17                   PIC 9(2).
               05  QSPC-RBEARLY-17                    PIC 9(2).
               05  QSPC-RBDEFER-17                    PIC 9(2).
               05  QSPC-RBPCT-17                      PIC 9(3)V9.
               05  QSPC-RBREDUC-17                    PIC 9(4)V99.
               05  QSPC-RBMIN-17                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-17                   PIC 9(2).
               05  QSPC-RBSPOPT2-17                   PIC 9(2).
               05  QSPC-RBMANAGER-17                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-17               PIC X.
               05  QSPC-RBFRMLA2-17                   PIC X(2).
               05  QSPC-RBFRMLA-18                    PIC X(2).
               05  QSPC-RBDTE-18-1                    PIC X.
               05  QSPC-RBDAYS-18-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-18-1                 PIC 9(4).
               05  QSPC-RBDTE-18-2                    PIC X.
               05  QSPC-RBDAYS-18-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-18-2                 PIC 9(4).
               05  QSPC-RBMINRET-18                   PIC 9999V99.
               05  QSPC-RBNOMON-18                    PIC 9(2).
               05  QSPC-RBDEDCHG-18                   PIC 9999V99.
               05  QSPC-RBRECAST-18                   PIC 9(2).
               05  QSPC-RBEARLY-18                    PIC 9(2).
               05  QSPC-RBDEFER-18                    PIC 9(2).
               05  QSPC-RBPCT-18                      PIC 9(3)V9.
               05  QSPC-RBREDUC-18                    PIC 9(4)V99.
               05  QSPC-RBMIN-18                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-18                   PIC 9(2).
               05  QSPC-RBSPOPT2-18                   PIC 9(2).
               05  QSPC-RBMANAGER-18                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-18               PIC X.
               05  QSPC-RBFRMLA2-18                   PIC X(2).
               05  QSPC-RBFRMLA-19                    PIC X(2).
               05  QSPC-RBDTE-19-1                    PIC X.
               05  QSPC-RBDAYS-19-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-19-1                 PIC 9(4).
               05  QSPC-RBDTE-19-2                    PIC X.
               05  QSPC-RBDAYS-19-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-19-2                 PIC 9(4).
               05  QSPC-RBMINRET-19                   PIC 9999V99.
               05  QSPC-RBNOMON-19                    PIC 9(2).
               05  QSPC-RBDEDCHG-19                   PIC 9999V99.
               05  QSPC-RBRECAST-19                   PIC 9(2).
               05  QSPC-RBEARLY-19                    PIC 9(2).
               05  QSPC-RBDEFER-19                    PIC 9(2).
               05  QSPC-RBPCT-19                      PIC 9(3)V9.
               05  QSPC-RBREDUC-19                    PIC 9(4)V99.
               05  QSPC-RBMIN-19                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-19                   PIC 9(2).
               05  QSPC-RBSPOPT2-19                   PIC 9(2).
               05  QSPC-RBMANAGER-19                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-19               PIC X.
               05  QSPC-RBFRMLA2-19                   PIC X(2).
               05  QSPC-RBFRMLA-20                    PIC X(2).
               05  QSPC-RBDTE-20-1                    PIC X.
               05  QSPC-RBDAYS-20-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-20-1                 PIC 9(4).
               05  QSPC-RBDTE-20-2                    PIC X.
               05  QSPC-RBDAYS-20-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-20-2                 PIC 9(4).
               05  QSPC-RBMINRET-20                   PIC 9999V99.
               05  QSPC-RBNOMON-20                    PIC 9(2).
               05  QSPC-RBDEDCHG-20                   PIC 9999V99.
               05  QSPC-RBRECAST-20                   PIC 9(2).
               05  QSPC-RBEARLY-20                    PIC 9(2).
               05  QSPC-RBDEFER-20                    PIC 9(2).
               05  QSPC-RBPCT-20                      PIC 9(3)V9.
               05  QSPC-RBREDUC-20                    PIC 9(4)V99.
               05  QSPC-RBMIN-20                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-20                   PIC 9(2).
               05  QSPC-RBSPOPT2-20                   PIC 9(2).
               05  QSPC-RBMANAGER-20                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-20               PIC X.
               05  QSPC-RBFRMLA2-20                   PIC X(2).
               05  QSPC-RBFRMLA-21                    PIC X(2).
               05  QSPC-RBDTE-21-1                    PIC X.
               05  QSPC-RBDAYS-21-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-21-1                 PIC 9(4).
               05  QSPC-RBDTE-21-2                    PIC X.
               05  QSPC-RBDAYS-21-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-21-2                 PIC 9(4).
               05  QSPC-RBMINRET-21                   PIC 9999V99.
               05  QSPC-RBNOMON-21                    PIC 9(2).
               05  QSPC-RBDEDCHG-21                   PIC 9999V99.
               05  QSPC-RBRECAST-21                   PIC 9(2).
               05  QSPC-RBEARLY-21                    PIC 9(2).
               05  QSPC-RBDEFER-21                    PIC 9(2).
               05  QSPC-RBPCT-21                      PIC 9(3)V9.
               05  QSPC-RBREDUC-21                    PIC 9(4)V99.
               05  QSPC-RBMIN-21                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-21                   PIC 9(2).
               05  QSPC-RBSPOPT2-21                   PIC 9(2).
               05  QSPC-RBMANAGER-21                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-21               PIC X.
               05  QSPC-RBFRMLA2-21                   PIC X(2).
               05  QSPC-RBFRMLA-22                    PIC X(2).
               05  QSPC-RBDTE-22-1                    PIC X.
               05  QSPC-RBDAYS-22-1                   PIC 9(2).
               05  QSPC-RBYRTYPE-22-1                 PIC 9(4).
               05  QSPC-RBDTE-22-2                    PIC X.
               05  QSPC-RBDAYS-22-2                   PIC 9(2).
               05  QSPC-RBYRTYPE-22-2                 PIC 9(4).
               05  QSPC-RBMINRET-22                   PIC 9999V99.
               05  QSPC-RBNOMON-22                    PIC 9(2).
               05  QSPC-RBDEDCHG-22                   PIC 9999V99.
               05  QSPC-RBRECAST-22                   PIC 9(2).
               05  QSPC-RBEARLY-22                    PIC 9(2).
               05  QSPC-RBDEFER-22                    PIC 9(2).
               05  QSPC-RBPCT-22                      PIC 9(3)V9.
               05  QSPC-RBREDUC-22                    PIC 9(4)V99.
               05  QSPC-RBMIN-22                      PIC 9(4)V99.
               05  QSPC-RBSPOPT1-22                   PIC 9(2).
               05  QSPC-RBSPOPT2-22                   PIC 9(2).
               05  QSPC-RBMANAGER-22                  PIC 9(2).
               05  QSPC-RBUNITPER-CD-22               PIC X.
               05  QSPC-RBFRMLA2-22                   PIC X(2).
               05  QSPC-RBCOMCD                       PIC X.
               05  QSPC-ADDON-DISFRMLA                PIC X.
               05  QSPC-ADDON-WHOLE-FG                PIC X.
               05  QSPC-ADDON-ORG-MATURITY            PIC X.
               05  QSPC-CONSOLIDATE-RT-FG             PIC X.
               05  QSPC-1STPY-ADDON-DAYS              PIC 9(3).
               05  QSPC-1STPY-ADDON-YRTYPE            PIC 9(3).
               05  QSPC-ADDON-RATE-FG                 PIC X.
               05  QSPC-ADDON-RATE                    PIC 999V999.
               05  QSPC-FEEFRMLA-1                    PIC X.
               05  QSPC-FEECD-1                       PIC XX.
               05  QSPC-FEEID-1                       PIC X(7).
               05  QSPC-FEEFRMLA-2                    PIC X.
               05  QSPC-FEECD-2                       PIC XX.
               05  QSPC-FEEID-2                       PIC X(7).
               05  QSPC-ROUND-PAY-FG                  PIC X.
               05  QSPC-IRREGULAR-PAY-FG              PIC X.
               05  QSPC-DLRVCHGS-RB-CD-1              PIC X.
               05  QSPC-DLRVCHGS-RB-CD-2              PIC X.
               05  QSPC-DLRVCHGS-RB-CD-3              PIC X.
               05  QSPC-PARVHELD-RB-CD                PIC X.
               05  QSPC-PARVPAID-RB-CD                PIC X.
               05  QSPC-CONTR-CREDIT                  PIC 99V99.
               05  QSPC-CONTR-CREDIT-CD               PIC X.
               05  QSPC-CONTR-CREDIT-MIN-DEL-PER      PIC 999V9.
               05  QSPC-OPT-CAL-CHGABLE               PIC X.
               05  QSPC-PBDEL-DAYS                    PIC S999.
               05  QSPC-PBINCR-PERCENT                PIC 999.
               05  QSPC-CPOT30-CD                     PIC X.
               05  QSPC-CPOTCHGOFF-CD                 PIC X.
               05  QSPC-RDPOT30-CD                    PIC X.
               05  QSPC-RDPOTCHGOFF-CD                PIC X.
               05  QSPC-MAX-ALLOW-DELINQ              PIC 99.
               05  QSPC-RESCIND-BUS-DAYS              PIC X.
               05  QSPC-RESCIND-SATURDAY              PIC X.
               05  QSPC-MIN-CREDIT-SCORE              PIC 9(4).
               05  QSPC-DUAL-IN-BRANCH                PIC X.
               05  QSPC-DUAL-IN-STATE                 PIC X.
               05  QSPC-PACESETTER-FG                 PIC X.
               05  QSPC-WAGE-ASSIGN-DAYS              PIC 99.
               05  QSPC-CAL-PAYMENT-ROUND-FG          PIC X.
               05  QSPC-CAN-LOANS-FG                  PIC X.
               05  QSPC-ALLOW-DELINQ-POLCD            PIC X.
               05  QSPC-YTD-MAX-ALLOW-DELINQ          PIC 99.
               05  QSPC-LNCOST-FRMLA                  PIC X.
               05  QSPC-LNCOST-AMT                    PIC 9(7)V99.
               05  QSPC-DLDISC-RVPRIN1-PCNT           PIC 9(3)V99.
               05  QSPC-RENEWABLE-PAYS                PIC S9(4).
               05  QSPC-RENEWABLE-DAYS                PIC S9(4).
               05  QSPC-ALLOW-LN-BACK-DATE            PIC X.
               05  QSPC-DLDISC-RVPRIN1-AF-PCNT        PIC 9(3)V99.
               05  QSPC-CAL-MAX-AT-ESTIMATE           PIC X.
               05  QSPC-INSAH-RETRO                   PIC X.
               05  QSPC-INSAH-DAYS-WAIT               PIC XX.
               05  QSPC-ADDON-EXCEPT-FG               PIC X.
               05  QSPC-REPO-TRCD-1                   PIC X(02).
               05  QSPC-REPO-FMNO-1-1                 PIC 9(03).
               05  QSPC-REPO-FMNO-1-2                 PIC 9(03).
               05  QSPC-REPO-FMNO-1-3                 PIC 9(03).
               05  QSPC-REPO-FMNO-1-4                 PIC 9(03).
               05  QSPC-REPO-TRCD-2                   PIC X(02).
               05  QSPC-REPO-FMNO-2-1                 PIC 9(03).
               05  QSPC-REPO-FMNO-2-2                 PIC 9(03).
               05  QSPC-REPO-FMNO-2-3                 PIC 9(03).
               05  QSPC-REPO-FMNO-2-4                 PIC 9(03).
               05  QSPC-REPO-TRCD-3                   PIC X(02).
               05  QSPC-REPO-FMNO-3-1                 PIC 9(03).
               05  QSPC-REPO-FMNO-3-2                 PIC 9(03).
               05  QSPC-REPO-FMNO-3-3                 PIC 9(03).
               05  QSPC-REPO-FMNO-3-4                 PIC 9(03).
               05  QSPC-CANCEL-ADDON-FG               PIC X.
               05  QSPC-DEFAULT-O2-INS-COMP           PIC 9(2).
               05  QSPC-LC-ON-JUDGEMENTS              PIC X.
               05  QSPC-DELQ-IB-AGE                   PIC 999.
               05  QSPC-DELQ-IB-MAX-DAYS              PIC 999.
               05  QSPC-INSURANCE-WRITTEN             PIC X.
               05  QSPC-INTERIM-COUPON                PIC X.
               05  QSPC-AUTO-ASSESS-LCHG              PIC X.
               05  QSPC-MAX-CASH-PROCEEDS             PIC 9(6)V99.
               05  QSPC-ALLOW-ZERO-PROC               PIC X.
               05  QSPC-BANKRULE-RATE                 PIC 9(4).
               05  QSPC-AUTO-SEND-TRCD-1              PIC X(02).
               05  QSPC-AUTO-SEND-FMNO-1-1            PIC 9(03).
               05  QSPC-AUTO-SEND-FMNO-1-2            PIC 9(03).
               05  QSPC-AUTO-SEND-FMNO-1-3            PIC 9(03).
               05  QSPC-AUTO-SEND-FMNO-1-4            PIC 9(03).
               05  QSPC-AUTO-SEND-TRCD-2              PIC X(02).
               05  QSPC-AUTO-SEND-FMNO-2-1            PIC 9(03).
               05  QSPC-AUTO-SEND-FMNO-2-2            PIC 9(03).
               05  QSPC-AUTO-SEND-FMNO-2-3            PIC 9(03).
               05  QSPC-AUTO-SEND-FMNO-2-4            PIC 9(03).
               05  QSPC-DLDISC-CONV-BF-PCNT           PIC 999V999.
               05  QSPC-UNITPER-CD                    PIC X.
               05  QSPC-RENEWAL-EDIT                  PIC X.
               05  QSPC-ROUND-MFEE                    PIC X.
               05  QSPC-ADV-TO-1ST-NO-CORR            PIC X.
               05  QSPC-LC-INT-ON-REPO-NON-PL         PIC X.
               05  QSPC-CONTRACTUAL-SET-NONACCRUAL    PIC 9(3).
               05  QSPC-CONTRACTUAL-RESET-ACCRUAL     PIC 9(3).
               05  QSPC-MONTHS-PAY-NAPP-COUNTER       PIC 99.
               05  QSPC-SHORT-PAY-THRESHOLD           PIC 9(3).
               05  QSPC-CASH-AVAIL-PERCENT            PIC 99.
               05  QSPC-O1-JOINT-FG                   PIC X.
               05  QSPC-NET-BAL-REQ                   PIC S9(3).
               05  QSPC-AMT-PROCEEDS-REQ              PIC S9(7).
               05  QSPC-ALT-PREPAY-MONTHS             PIC 9(2).
               05  QSPC-PERCENT-REGPAY-AVAIL          PIC 9(3).
               05  QSPC-ALT-DEFFRMLA                  PIC 99.
               05  QSPC-ALT-DEFMIN                    PIC 99V99.
               05  QSPC-ALT-DEFMAX                    PIC 9999V99.
               05  QSPC-NON-LOAN-FLAG                 PIC X.
               05  QSPC-SKIP-TRW                      PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.

      *=================================================================
       01  show-counts.
           03  filler                  PIC x(19)     value spaces.
           03  show-io-counts          PIC zzz,zzz,zzz,zz9.
           03  filler                  PIC x(3)      value " | ".
           03  show-io-table           PIC x(7).

       01  show-status.
           03  show-iteration          PIC 9(6).
           03  filler                  PIC x(4)      value " of ".
           03  show-max                PIC 9(6).
           03  filler                  PIC x(3)      value " | ".
           03  show-io-count           PIC zzz,zzz,zzz,zz9.
           03  filler                  PIC x(3)      value " | ".
           03  show-time               PIC 99/99/99.

       01  iteration-count             pic 9(9)      value zeroes.
       78  iteration-max                             value 25.

      *-----------------------------------------------------------------
       01  system-buf.
           03  system-buf-cmd          pic x(120)    value spaces.
           03  system-buf-output       pic x(04)     value " >  ".
           03  system-buf-output-file  pic x(35)     value spaces.

       01  system-output-file.
           03  filler                  pic x(09)     value "/usr/tmp/".
           03  filler                  pic x(07)     value "ACUMLT-".
           03  system-output-date      pic 9(08)     value zeroes.
           03  filler                  pic x(01)     value "-".
           03  system-output-time      pic 9(06)     value zeroes.

       01  system-echo-cmd.
           03  filler                  pic x(05)     value "echo ".
           03  filler                  pic x(01)     value X"22".
           03  system-echo-text        pic x(80)     value spaces.
           03  filler                  pic x(01)     value X"22".

      *-----------------------------------------------------------------
       01  time-now.
           03  cur-hh          pic 99.
           03  cur-mm          pic 99.
           03  cur-ss          pic 99.
           03  cur-hd          pic 99.
       01  time-now2           redefines time-now.
           03  h-and-m         pic 9(4).
           03  filler          pic 9(4).
       01  time-now3           redefines time-now.
           03  hh-mm-ss        pic 9(6).
           03  filler          pic 9(2).
       01  time-edit.
           03  t-hh            pic 99.
           03  t-col           pic x.
           03  t-mm            pic 99.
           03  t-space         pic x.
           03  t-ampm          pic xx.

       01  time-military-fg    pic x    value spaces.

      *=================================================================
       01  current-sp1-key.
           05  current-sp-orgst                   PIC XX.
           05  current-sp-sprclass                PIC 999.
           05  current-sp-subclass                PIC 99.
           05  current-sp-lawcode                 PIC 99.

       01  e-msg4                    pic x(4).
       01  io-fg                     pic 9(1)    value zeroes.
       01  io-count-bwfile           pic 9(12)   value zeroes.
       01  io-count-lnfile           pic 9(12)   value zeroes.
       01  io-count-lpfile           pic 9(12)   value zeroes.
       01  io-count-spafile          pic 9(12)   value zeroes.
       01  io-count-spbfile          pic 9(12)   value zeroes.
       01  io-count-spcfile          pic 9(12)   value zeroes.
       01  io-count-total            pic 9(12)   value zeroes.
       01  main-stat                 pic 9(1)    value zeroes.

       78  stat---good                           value "00".
       78  stat---bad                            value "xx".
       01  stat                      pic x(2)    value spaces.
           88  stat-valid                        value "00".
           88  stat-good                         value "00".
           88  stat-bad                          value "  " thru "//"
                                                       "01" thru "~~".

       01  sub                       pic 9(2)    value zeroes.

      *- - -{standard cursor for start & read-next}
       01  ln1-cursor-stat           pic x(2)    value spaces.
           88  ln1-cursor-stat-good              value "00".
           88  ln1-cursor-stat-bad               value "  " thru "//"
                                                       "01" thru "~~".

       01  lp1-cursor-stat           pic x(2)    value spaces.
           88  lp1-cursor-stat-good              value "00".
           88  lp1-cursor-stat-bad               value "  " thru "//"
                                                       "01" thru "~~".

      ******************************************************************
       PROCEDURE DIVISION.
       Main Section.
           EXEC SQL WHENEVER SQLERROR GO TO Error-Exit END-EXEC.
           Display "This program demonstrates memory usage while using S
      -    "QL table(s)" at line 1.
      *kc  perform initialization-routine.
           Perform make-connection.
           perform execute-it.
           perform disconnect-connection.
           Display "Program Complete. Press Enter to Exit." at line 10.
      *kc  accept omitted.
           stop run.

       execute-it.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    initialize output file
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ACCEPT system-output-date from century-date.
           perform get-time.
           move hh-mm-ss           to system-output-time.
           move system-output-file to system-buf-output-file.

           move all "=" to system-echo-text.
           move system-echo-cmd to system-buf-cmd.
           call 'SYSTEM' using system-buf.

           move " >> " to system-buf-output.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           display " " at line 3.
           display "OUTPUT FILE: " system-output-file.

           move zeroes to io-count-bwfile
                          io-count-lnfile
                          io-count-lpfile
                          io-count-spafile
                          io-count-spbfile
                          io-count-spcfile
                          io-count-total
                          iteration-count.
           perform display-current-status.

           perform io-iteration varying iteration-count from 1 by 1
                                  until iteration-count > iteration-max.

      *=================================================================
       display-current-status.

           move iteration-count to show-iteration.
           move iteration-max   to show-max.
           compute io-count-total = io-count-bwfile
                                  + io-count-lnfile
                                  + io-count-lpfile
                                  + io-count-spafile
                                  + io-count-spbfile
                                  + io-count-spcfile.
           move io-count-total  to show-io-count.

           perform get-time.
           move hh-mm-ss to show-time.
           inspect show-time replacing all "/" by ":".

      *kc  call 'SYSTEM' using 'clear'
           display " " at line 5.
           display "ITERATION  : " show-status.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           move show-status     to system-echo-text.
           move system-echo-cmd to system-buf-cmd.
           call 'SYSTEM' using system-buf.

           move "bwfile"         to show-io-table.
           move io-count-bwfile  to show-io-counts.
           move show-counts      to system-echo-text.
           move system-echo-cmd  to system-buf-cmd.
           call 'SYSTEM' using system-buf.

           move "lnfile"         to show-io-table.
           move io-count-lnfile  to show-io-counts.
           move show-counts      to system-echo-text.
           move system-echo-cmd  to system-buf-cmd.
           call 'SYSTEM' using system-buf.

           move "lpfile"         to show-io-table.
           move io-count-lpfile  to show-io-counts.
           move show-counts      to system-echo-text.
           move system-echo-cmd  to system-buf-cmd.
           call 'SYSTEM' using system-buf.

           move "spafile"        to show-io-table.
           move io-count-spafile to show-io-counts.
           move show-counts      to system-echo-text.
           move system-echo-cmd  to system-buf-cmd.
           call 'SYSTEM' using system-buf.

           move "spbfile"        to show-io-table.
           move io-count-spbfile to show-io-counts.
           move show-counts      to system-echo-text.
           move system-echo-cmd  to system-buf-cmd.
           call 'SYSTEM' using system-buf.

           move "spcfile"        to show-io-table.
           move io-count-spcfile to show-io-counts.
           move show-counts      to system-echo-text.
           move system-echo-cmd  to system-buf-cmd.
           call 'SYSTEM' using system-buf.

      *    move "free"      to system-buf-cmd.
      *    call 'SYSTEM' using system-buf.

      *    move all "-" to system-echo-text.
      *    move system-echo-cmd to system-buf-cmd.
      *    call 'SYSTEM' using system-buf.

           move "sh /usr/local/bin/acumlt.sh" to system-buf-cmd.
           move spaces                        to system-buf-output.
           call 'SYSTEM' using system-buf.
           move " >> "                        to system-buf-output.

      *-----------------------------------------------------------------
       io-iteration.

           move spaces to current-sp-orgst.
           move zeroes to current-sp-sprclass
                          current-sp-subclass
                          current-sp-lawcode.

           perform start-ln1-file.

           if ( io-fg not = 0 )
              exit paragraph.

           move 0 to main-stat.   *> 0=Good; 1~9=Bad

           perform until main-stat not = 0
                   perform read-ln1-file-next

                   if ( io-fg not = 0 )      *> 0=Good; 1~9=Bad
                      move 9 to main-stat    *> 0=Good; 1~9=Bad
                   else
                      add 01 to io-count-lnfile
                      perform io-bwfile
                      perform io-spfile
                      perform io-lpfile
                   end-if
           end-perform.

           perform display-current-status.

      *-----------------------------------------------------------------
       io-bwfile.

           if ( qln-ssno1 not = zeroes )
              move qln-ownbr to qbw-ownbr
              move qln-ssno1 to qbw-ssno
              perform read-bw1-file
              add 01 to io-count-bwfile.

           if ( qln-ssno2 not = zeroes )
              move qln-ownbr to qbw-ownbr
              move qln-ssno2 to qbw-ssno
              perform read-bw1-file
              add 01 to io-count-bwfile.

           if ( qln-ssno3 not = zeroes )
              move qln-ownbr to qbw-ownbr
              move qln-ssno3 to qbw-ssno
              perform read-bw1-file
              add 01 to io-count-bwfile.

           if ( qln-ssno4 not = zeroes )
              move qln-ownbr to qbw-ownbr
              move qln-ssno4 to qbw-ssno
              perform read-bw1-file
              add 01 to io-count-bwfile.

      *-----------------------------------------------------------------
       io-spfile.

           if ( qln-orgst    = current-sp-orgst    ) and
              ( qln-sprclass = current-sp-sprclass ) and
              ( qln-subclass = current-sp-subclass ) and
              ( qln-lawcode  = current-sp-lawcode  )
              exit paragraph.

           move qln-orgst    to qspa-orgst qspb-orgst qspc-orgst
                                current-sp-orgst.
           move qln-sprclass to qspa-sprclass qspb-sprclass
                                qspc-sprclass current-sp-sprclass.
           move qln-subclass to qspa-subclass qspb-subclass
                                qspc-subclass current-sp-subclass.
           move qln-lawcode  to qspa-lawcode qspb-lawcode qspc-lawcode
                                current-sp-lawcode.

           perform read-spa1-file.

           if ( io-fg = 0 )      *> 0=Good; 1~9=Bad
              add 01 to io-count-spafile.

           perform read-spb1-file.

           if ( io-fg = 0 )      *> 0=Good; 1~9=Bad
              add 01 to io-count-spbfile.

           perform read-spc1-file.

           if ( io-fg = 0 )      *> 0=Good; 1~9=Bad
              add 01 to io-count-spcfile.

      *-----------------------------------------------------------------
       io-lpfile.

           move qln-ownbr  to qlp-brno.
           move qln-acctno to qlp-acctno.
           move zeroes     to qlp-seqno.
           perform start-lp1-file.

           if ( io-fg not = 0 )
              exit paragraph.

           perform until io-fg not = 0
              perform read-lp1-file-next

              if ( io-fg      = 0          ) and   *> 0=Good; 1~9=Bad
                 ( qlp-brno   = qln-ownbr  ) and
                 ( qlp-acctno = qln-acctno )
                 add 01 to io-count-lpfile
              end-if
           end-perform.

      *-----------------------------------------------------------------
       get-time.
           accept time-now from time.
           if time-military-fg = "y"
              move " " to t-ampm
           else
              if h-and-m not greater than 1159
                 move "am" to t-ampm
              else
                 move "pm" to t-ampm.
           if time-military-fg = " "
              if h-and-m is not less than 1300
                 subtract 1200 from h-and-m.
           if time-military-fg = " "
              if cur-hh = 0
                 move 12 to cur-hh.
           move cur-hh to t-hh.
           move cur-mm to t-mm.
           move ":" to t-col.
           move " " to t-space.

      *-----------------------------------------------------------------
       read-bw1-file.
           move "read" to e-msg4.
           move 0 to io-fg.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            INTO :QBW-REC
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           perform sql-io-validation.

      *-----------------------------------------------------------------
       start-ln1-file.
           move "star" to e-msg4.
           move 0 to io-fg.

           if ( ln1-cursor-stat-good )
              perform close-ln1-file.

           exec sql
            declare ln1_cursor cursor for
            select lnfile.ln_ownbr,
                   lnfile.ln_acctno,
                   lnfile.ln_class,
                   lnfile.ln_subclass,
                   lnfile.ln_lawcode,
                   lnfile.ln_ssno1,
                   isnull(lnfile.ln_makercd1,''),
                   lnfile.ln_ssno2,
                   isnull(lnfile.ln_makercd2,''),
                   lnfile.ln_ssno3,
                   isnull(lnfile.ln_makercd3,''),
                   lnfile.ln_ssno4,
                   isnull(lnfile.ln_makercd4,''),
                   isnull(lnfile.ln_orgst,''),
                   lnfile.ln_orgbr,
                   isnull(lnfile.ln_ownst,''),
                   lnfile.ln_sprclass,
                   isnull(lnfile.ln_convercd,''),
                   isnull(lnfile.ln_midstream,''),
                   isnull(lnfile.ln_purcd,''),
                   isnull(lnfile.ln_srcd,''),
                   isnull(lnfile.ln_collcd,''),
                   isnull(lnfile.ln_collid,''),
                   isnull(lnfile.ln_pocd,''),
                   isnull(lnfile.ln_plcd,''),
                   isnull(lnfile.ln_pltype,''),
                   isnull(lnfile.ln_confidentcd,''),
                   isnull(lnfile.ln_allotcd,''),
                   isnull(lnfile.ln_recastcd,''),
                   isnull(lnfile.ln_skip_trw,''),
                   isnull(lnfile.ln_statusfg,''),
                   isnull(lnfile.ln_smalloan,''),
                   isnull(lnfile.ln_salefin,''),
                   isnull(lnfile.ln_servacct,''),
                   isnull(lnfile.ln_loantype,''),
                   isnull(lnfile.ln_addon_fg,''),
                   isnull(lnfile.ln_pay_schld_fg,''),
                   lnfile.ln_lc_code,
                   isnull(lnfile.ln_can_loan_no,''),
                   isnull(lnfile.ln_statflags,''),
                   isnull(cast(lnfile.ln_entdate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_loandate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_purdate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_intdate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_cashdate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_filedate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_rescission_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_part_recourse_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_orig_1stpydate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_1stpydate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_jddate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_pldate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_pl_trdate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_acceldate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_redudate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_poffdate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_po_trdate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_legaldate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_bnkrptdate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_bnkrpt_dismiss_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_bnkrpt_discharge_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_bnkrpt_reaffirm_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_debtaidate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_repodate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_redeemdate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_solddate as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_ins_confirm_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_prior_rn_for10_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_last_rn_for10_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_trw_dofd_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_last_pypa_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_bt_trdate
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_metro2_last_pmt_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_followup_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_last_accrual_date
                                                  as varchar(10)),'0'),
                   lnfile.ln_aprate,
                   lnfile.ln_effrate,
                   lnfile.ln_smprate,
                   lnfile.ln_yldrate,
                   lnfile.ln_ovrdrate,
                   lnfile.ln_endorate,
                   lnfile.ln_jdrate,
                   lnfile.ln_accrate,
                   lnfile.ln_redurate,
                   lnfile.ln_conrate,
                   lnfile.ln_servper,
                   lnfile.ln_sellpr,
                   lnfile.ln_downpy,
                   lnfile.ln_tradein,
                   lnfile.ln_lnamt,
                   lnfile.ln_finchg,
                   lnfile.ln_intchg,
                   lnfile.ln_serchg,
                   lnfile.ln_extchg,
                   lnfile.ln_maintfee,
                   lnfile.ln_points,
                   lnfile.ln_proceeds,
                   lnfile.ln_orgterm,
                   lnfile.ln_1stpyamt,
                   lnfile.ln_noregpy,
                   lnfile.ln_regpyamt,
                   lnfile.ln_lastpyamt,
                   lnfile.ln_procash,
                   lnfile.ln_tillno,
                   isnull(lnfile.ln_state_flag,''),
                   lnfile.ln_state_worker,
                   lnfile.ln_state_worker2,
                   isnull(lnfile.ln_feecode_1,''),
                   lnfile.ln_feeamt_1,
                   isnull(lnfile.ln_feeours_1,''),
                   isnull(lnfile.ln_feefinchg_1,''),
                   isnull(lnfile.ln_feecode_2,''),
                   lnfile.ln_feeamt_2,
                   isnull(lnfile.ln_feeours_2,''),
                   isnull(lnfile.ln_feefinchg_2,''),
                   isnull(lnfile.ln_feecode_3,''),
                   lnfile.ln_feeamt_3,
                   isnull(lnfile.ln_feeours_3,''),
                   isnull(lnfile.ln_feefinchg_3,''),
                   isnull(lnfile.ln_feecode_4,''),
                   lnfile.ln_feeamt_4,
                   isnull(lnfile.ln_feeours_4,''),
                   isnull(lnfile.ln_feefinchg_4,''),
                   isnull(lnfile.ln_feecode_5,''),
                   lnfile.ln_feeamt_5,
                   isnull(lnfile.ln_feeours_5,''),
                   isnull(lnfile.ln_feefinchg_5,''),
                   isnull(lnfile.ln_feecode_6,''),
                   lnfile.ln_feeamt_6,
                   isnull(lnfile.ln_feeours_6,''),
                   isnull(lnfile.ln_feefinchg_6,''),
                   isnull(lnfile.ln_feecode_7,''),
                   lnfile.ln_feeamt_7,
                   isnull(lnfile.ln_feeours_7,''),
                   isnull(lnfile.ln_feefinchg_7,''),
                   isnull(lnfile.ln_feecode_8,''),
                   lnfile.ln_feeamt_8,
                   isnull(lnfile.ln_feeours_8,''),
                   isnull(lnfile.ln_feefinchg_8,''),
                   isnull(lnfile.ln_feecode_9,''),
                   lnfile.ln_feeamt_9,
                   isnull(lnfile.ln_feeours_9,''),
                   isnull(lnfile.ln_feefinchg_9,''),
                   isnull(lnfile.ln_feecode_10,''),
                   lnfile.ln_feeamt_10,
                   isnull(lnfile.ln_feeours_10,''),
                   isnull(lnfile.ln_feefinchg_10,''),
                   isnull(lnfile.ln_feecode_11,''),
                   lnfile.ln_feeamt_11,
                   isnull(lnfile.ln_feeours_11,''),
                   isnull(lnfile.ln_feefinchg_11,''),
                   isnull(lnfile.ln_feecode_12,''),
                   lnfile.ln_feeamt_12,
                   isnull(lnfile.ln_feeours_12,''),
                   isnull(lnfile.ln_feefinchg_12,''),
                   isnull(lnfile.ln_feecode_13,''),
                   lnfile.ln_feeamt_13,
                   isnull(lnfile.ln_feeours_13,''),
                   isnull(lnfile.ln_feefinchg_13,''),
                   isnull(lnfile.ln_feecode_14,''),
                   lnfile.ln_feeamt_14,
                   isnull(lnfile.ln_feeours_14,''),
                   isnull(lnfile.ln_feefinchg_14,''),
                   isnull(lnfile.ln_feecode_15,''),
                   lnfile.ln_feeamt_15,
                   isnull(lnfile.ln_feeours_15,''),
                   isnull(lnfile.ln_feefinchg_15,''),
                   isnull(lnfile.ln_feecode_16,''),
                   lnfile.ln_feeamt_16,
                   isnull(lnfile.ln_feeours_16,''),
                   isnull(lnfile.ln_feefinchg_16,''),
                   isnull(lnfile.ln_feecode_17,''),
                   lnfile.ln_feeamt_17,
                   isnull(lnfile.ln_feeours_17,''),
                   isnull(lnfile.ln_feefinchg_17,''),
                   isnull(lnfile.ln_feecode_18,''),
                   lnfile.ln_feeamt_18,
                   isnull(lnfile.ln_feeours_18,''),
                   isnull(lnfile.ln_feefinchg_18,''),
                   isnull(lnfile.ln_feecode_19,''),
                   lnfile.ln_feeamt_19,
                   isnull(lnfile.ln_feeours_19,''),
                   isnull(lnfile.ln_feefinchg_19,''),
                   isnull(lnfile.ln_feecode_20,''),
                   lnfile.ln_feeamt_20,
                   isnull(lnfile.ln_feeours_20,''),
                   isnull(lnfile.ln_feefinchg_20,''),
                   isnull(lnfile.ln_insured,''),
                   isnull(lnfile.ln_instypes,''),
                   isnull(lnfile.ln_inseligible,''),
                   lnfile.ln_inscomp_1,
                   isnull(lnfile.ln_insours_1,''),
                   isnull(cast(lnfile.ln_inseff_date_1
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_1
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_1,
                   lnfile.ln_inscovr_1,
                   lnfile.ln_inscomm_1,
                   lnfile.ln_insanticern_1,
                   lnfile.ln_insanticadj_1,
                   lnfile.ln_insufern_1,
                   lnfile.ln_insaccern_1,
                   lnfile.ln_insearn_order_1,
                   isnull(lnfile.ln_insfinchg_1,''),
                   lnfile.ln_inscomp_2,
                   isnull(lnfile.ln_insours_2,''),
                   isnull(cast(lnfile.ln_inseff_date_2
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_2
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_2,
                   lnfile.ln_inscovr_2,
                   lnfile.ln_inscomm_2,
                   lnfile.ln_insanticern_2,
                   lnfile.ln_insanticadj_2,
                   lnfile.ln_insufern_2,
                   lnfile.ln_insaccern_2,
                   lnfile.ln_insearn_order_2,
                   isnull(lnfile.ln_insfinchg_2,''),
                   lnfile.ln_inscomp_3,
                   isnull(lnfile.ln_insours_3,''),
                   isnull(cast(lnfile.ln_inseff_date_3
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_3
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_3,
                   lnfile.ln_inscovr_3,
                   lnfile.ln_inscomm_3,
                   lnfile.ln_insanticern_3,
                   lnfile.ln_insanticadj_3,
                   lnfile.ln_insufern_3,
                   lnfile.ln_insaccern_3,
                   lnfile.ln_insearn_order_3,
                   isnull(lnfile.ln_insfinchg_3,''),
                   lnfile.ln_inscomp_4,
                   isnull(lnfile.ln_insours_4,''),
                   isnull(cast(lnfile.ln_inseff_date_4
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_4
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_4,
                   lnfile.ln_inscovr_4,
                   lnfile.ln_inscomm_4,
                   lnfile.ln_insanticern_4,
                   lnfile.ln_insanticadj_4,
                   lnfile.ln_insufern_4,
                   lnfile.ln_insaccern_4,
                   lnfile.ln_insearn_order_4,
                   isnull(lnfile.ln_insfinchg_4,''),
                   lnfile.ln_inscomp_5,
                   isnull(lnfile.ln_insours_5,''),
                   isnull(cast(lnfile.ln_inseff_date_5
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_5
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_5,
                   lnfile.ln_inscovr_5,
                   lnfile.ln_inscomm_5,
                   lnfile.ln_insanticern_5,
                   lnfile.ln_insanticadj_5,
                   lnfile.ln_insufern_5,
                   lnfile.ln_insaccern_5,
                   lnfile.ln_insearn_order_5,
                   isnull(lnfile.ln_insfinchg_5,''),
                   lnfile.ln_inscomp_6,
                   isnull(lnfile.ln_insours_6,''),
                   isnull(cast(lnfile.ln_inseff_date_6
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_6
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_6,
                   lnfile.ln_inscovr_6,
                   lnfile.ln_inscomm_6,
                   lnfile.ln_insanticern_6,
                   lnfile.ln_insanticadj_6,
                   lnfile.ln_insufern_6,
                   lnfile.ln_insaccern_6,
                   lnfile.ln_insearn_order_6,
                   isnull(lnfile.ln_insfinchg_6,''),
                   lnfile.ln_inscomp_7,
                   isnull(lnfile.ln_insours_7,''),
                   isnull(cast(lnfile.ln_inseff_date_7
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_7
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_7,
                   lnfile.ln_inscovr_7,
                   lnfile.ln_inscomm_7,
                   lnfile.ln_insanticern_7,
                   lnfile.ln_insanticadj_7,
                   lnfile.ln_insufern_7,
                   lnfile.ln_insaccern_7,
                   lnfile.ln_insearn_order_7,
                   isnull(lnfile.ln_insfinchg_7,''),
                   lnfile.ln_inscomp_8,
                   isnull(lnfile.ln_insours_8,''),
                   isnull(cast(lnfile.ln_inseff_date_8
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_8
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_8,
                   lnfile.ln_inscovr_8,
                   lnfile.ln_inscomm_8,
                   lnfile.ln_insanticern_8,
                   lnfile.ln_insanticadj_8,
                   lnfile.ln_insufern_8,
                   lnfile.ln_insaccern_8,
                   lnfile.ln_insearn_order_8,
                   isnull(lnfile.ln_insfinchg_8,''),
                   lnfile.ln_inscomp_9,
                   isnull(lnfile.ln_insours_9,''),
                   isnull(cast(lnfile.ln_inseff_date_9
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_9
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_9,
                   lnfile.ln_inscovr_9,
                   lnfile.ln_inscomm_9,
                   lnfile.ln_insanticern_9,
                   lnfile.ln_insanticadj_9,
                   lnfile.ln_insufern_9,
                   lnfile.ln_insaccern_9,
                   lnfile.ln_insearn_order_9,
                   isnull(lnfile.ln_insfinchg_9,''),
                   lnfile.ln_inscomp_10,
                   isnull(lnfile.ln_insours_10,''),
                   isnull(cast(lnfile.ln_inseff_date_10
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_insexp_date_10
                                                  as varchar(10)),'0'),
                   lnfile.ln_insprem_10,
                   lnfile.ln_inscovr_10,
                   lnfile.ln_inscomm_10,
                   lnfile.ln_insanticern_10,
                   lnfile.ln_insanticadj_10,
                   lnfile.ln_insufern_10,
                   lnfile.ln_insaccern_10,
                   lnfile.ln_insearn_order_10,
                   isnull(lnfile.ln_insfinchg_10,''),
                   lnfile.ln_py_yyyymm_recdel,
                   lnfile.ln_mtd_pays_recdel,
                   isnull(lnfile.ln_garnish_extr,''),
                   isnull(lnfile.ln_ship_same_fg,''),
                   lnfile.ln_ad_d_ins_covr,
                   lnfile.ln_tot_partial_chgoff,
                   isnull(lnfile.ln_lpo,''),
                   isnull(lnfile.ln_rep_cd,''),
                   lnfile.ln_emp_id,
                   isnull(lnfile.ln_process_cd,''),
                   lnfile.ln_wcbc_creditlim,
                   isnull(lnfile.ln_followup_cd,''),
                   isnull(lnfile.ln_mip_fg,''),
                   lnfile.ln_mip_mtd_clahpaid,
                   isnull(cast(lnfile.ln_mip_clpdth_date
                                                  as varchar(10)),'0'),
                   lnfile.ln_mip_tot_clpaid,
                   isnull(cast(lnfile.ln_mip_ahpdth_date
                                                  as varchar(10)),'0'),
                   lnfile.ln_mip_tot_ahpaid,
                   lnfile.ln_il_gross_income,
                   lnfile.ln_il_gross_income2,
                   isnull(lnfile.ln_rescission,''),
                   isnull(lnfile.ln_interim_trw,''),
                   lnfile.ln_dlno,
                   lnfile.ln_dlpool,
                   isnull(lnfile.ln_dlrecourse,''),
                   lnfile.ln_dlpartrcamt,
                   lnfile.ln_origpuramt,
                   lnfile.ln_purbal,
                   lnfile.ln_dlrvprin_1,
                   lnfile.ln_dlrvprin_2,
                   lnfile.ln_dlrvprin_3,
                   lnfile.ln_dlrvchgs_1,
                   lnfile.ln_dlrvchgs_2,
                   lnfile.ln_dlrvchgs_3,
                   lnfile.ln_dlparvheld,
                   lnfile.ln_dlparvpaid,
                   lnfile.ln_dldisc,
                   isnull(cast(lnfile.ln_lcpdth_date
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_intpdth_date
                                                  as varchar(10)),'0'),
                   lnfile.ln_curbal,
                   lnfile.ln_intbal,
                   lnfile.ln_lcbal,
                   lnfile.ln_othbal,
                   lnfile.ln_ot2bal,
                   lnfile.ln_lcpaid,
                   lnfile.ln_lcpartials,
                   lnfile.ln_stmt_counter,
                   lnfile.ln_prior_stmt_counter,
                   isnull(lnfile.ln_frozen_reason,''),
                   isnull(lnfile.ln_coupon_statement,''),
                   lnfile.ln_monthend_bal,
                   lnfile.ln_anticterm,
                   lnfile.ln_anticern_1,
                   lnfile.ln_anticadj_1,
                   lnfile.ln_ufern_1,
                   lnfile.ln_accern_1,
                   lnfile.ln_earn_order_1,
                   lnfile.ln_anticern_2,
                   lnfile.ln_anticadj_2,
                   lnfile.ln_ufern_2,
                   lnfile.ln_accern_2,
                   lnfile.ln_earn_order_2,
                   lnfile.ln_anticern_3,
                   lnfile.ln_anticadj_3,
                   lnfile.ln_ufern_3,
                   lnfile.ln_accern_3,
                   lnfile.ln_earn_order_3,
                   lnfile.ln_anticern_4,
                   lnfile.ln_anticadj_4,
                   lnfile.ln_ufern_4,
                   lnfile.ln_accern_4,
                   lnfile.ln_earn_order_4,
                   lnfile.ln_anticern_5,
                   lnfile.ln_anticadj_5,
                   lnfile.ln_ufern_5,
                   lnfile.ln_accern_5,
                   lnfile.ln_earn_order_5,
                   lnfile.ln_anticern_6,
                   lnfile.ln_anticadj_6,
                   lnfile.ln_ufern_6,
                   lnfile.ln_accern_6,
                   lnfile.ln_earn_order_6,
                   lnfile.ln_anticern_7,
                   lnfile.ln_anticadj_7,
                   lnfile.ln_ufern_7,
                   lnfile.ln_accern_7,
                   lnfile.ln_earn_order_7,
                   lnfile.ln_anticern_8,
                   lnfile.ln_anticadj_8,
                   lnfile.ln_ufern_8,
                   lnfile.ln_accern_8,
                   lnfile.ln_earn_order_8,
                   lnfile.ln_anticern_9,
                   lnfile.ln_anticadj_9,
                   lnfile.ln_ufern_9,
                   lnfile.ln_accern_9,
                   lnfile.ln_earn_order_9,
                   lnfile.ln_anticern_10,
                   lnfile.ln_anticadj_10,
                   lnfile.ln_ufern_10,
                   lnfile.ln_accern_10,
                   lnfile.ln_earn_order_10,
                   lnfile.ln_lmo_prior_interest,
                   lnfile.ln_prior_unearned,
                   isnull(lnfile.ln_erndelq_stop,''),
                   lnfile.ln_nonacrern,
                   lnfile.ln_prior_nonacrern,
                   isnull(cast(lnfile.ln_ern_ib_pdth_date
                                                  as varchar(10)),'0'),
                   lnfile.ln_ern_ib_prinbal,
                   lnfile.ln_ern_ib_intowe,
                   lnfile.ln_totern,
                   lnfile.ln_tototh,
                   lnfile.ln_totint,
                   lnfile.ln_totlchg,
                   lnfile.ln_totdef,
                   lnfile.ln_totseracct,
                   lnfile.ln_lcwaivers,
                   lnfile.ln_intwaivers,
                   lnfile.ln_totpaymntd,
                   lnfile.ln_totexcpaymntd,
                   lnfile.ln_totnodef,
                   lnfile.ln_ytdnodef,
                   lnfile.ln_totnodefact,
                   lnfile.ln_ytdnodefact,
                   lnfile.ln_totnoaldel,
                   lnfile.ln_ytdnoaldel,
                   lnfile.ln_ytdintpd,
                   lnfile.ln_lyrintpd,
                   lnfile.ln_totintpd,
                   lnfile.ln_recastint,
                   lnfile.ln_recastprin,
                   isnull(lnfile.ln_cl_rebate,''),
                   isnull(lnfile.ln_ah_rebate,''),
                   isnull(lnfile.ln_pp_rebate,''),
                   isnull(lnfile.ln_int_rebate,''),
                   isnull(lnfile.ln_sc_rebate,''),
                   isnull(lnfile.ln_mf_rebate,''),
                   isnull(lnfile.ln_df_rebate,''),
                   isnull(lnfile.ln_ex_rebate,''),
                   isnull(lnfile.ln_o1_rebate,''),
                   isnull(lnfile.ln_o2_rebate,''),
                   isnull(lnfile.ln_o3_rebate,''),
                   isnull(lnfile.ln_o4_rebate,''),
                   isnull(lnfile.ln_o5_rebate,''),
                   isnull(lnfile.ln_o6_rebate,''),
                   isnull(lnfile.ln_o7_rebate,''),
                   isnull(lnfile.ln_un_o7_reb,''),
                   isnull(lnfile.ln_un_o8_reb,''),
                   isnull(lnfile.ln_un_o9_reb,''),
                   isnull(lnfile.ln_un_o10_reb,''),
                   isnull(lnfile.ln_plreason,''),
                   isnull(lnfile.ln_collector,''),
                   isnull(lnfile.ln_actioncd,''),
                   isnull(lnfile.ln_actioncd2,''),
                   isnull(lnfile.ln_atcode,''),
                   isnull(lnfile.ln_recode,''),
                   isnull(lnfile.ln_escrow_fg,''),
                   lnfile.ln_promdays_pastdue,
                   isnull(lnfile.ln_promid,''),
                   isnull(cast(lnfile.ln_promdate as varchar(10)),'0'),
                   lnfile.ln_promamt,
                   isnull(cast(lnfile.ln_promdate2
                                                  as varchar(10)),'0'),
                   isnull(cast(lnfile.ln_date_paid_last
                                                  as varchar(10)),'0'),
                   lnfile.ln_amt_paid_last,
                   lnfile.ln_lastcformno,
                   isnull(cast(lnfile.ln_lastcformdate
                                                  as varchar(10)),'0'),
                   lnfile.ln_delcnt,
                   lnfile.ln_delcnt2,
                   lnfile.ln_creditlim,
                   lnfile.ln_crscore,
                   lnfile.ln_lastsformno,
                   isnull(cast(lnfile.ln_lastsformdate
                                                  as varchar(10)),'0'),
                   lnfile.ln_solcnt,
                   isnull(lnfile.ln_message,''),
                   lnfile.ln_oldacctno,
                   lnfile.ln_prlnno,
                   lnfile.ln_prbal,
                   lnfile.ln_printdue,
                   lnfile.ln_prfinchgrb,
                   lnfile.ln_prinsrb,
                   isnull(lnfile.ln_prpocd,''),
                   isnull(cast(lnfile.ln_prpodate as varchar(10)),'0'),
                   lnfile.ln_prdelinqamt,
                   lnfile.ln_prlnamt,
                   lnfile.ln_prpdue_days,
                   lnfile.ln_prdldisc_unearn,
                   lnfile.ln_prconprofile,
                   lnfile.ln_sum_origpuramt,
                   lnfile.ln_sum_lnamt,
                   lnfile.ln_sum_finchg,
                   lnfile.ln_sum_intchg,
                   lnfile.ln_sum_orgterm,
                   lnfile.ln_sum_regpyamt,
                   lnfile.ln_sum_extchg,
                   lnfile.ln_sum_serchg,
                   lnfile.ln_sum_maintfee,
                   isnull(lnfile.ln_unitper_cd,''),
                   lnfile.ln_unitper_freq,
                   lnfile.ln_days_in_unitper,
                   lnfile.ln_unitper_per_year,
                   isnull(lnfile.ln_repocd,''),
                   isnull(lnfile.ln_br_tfr_fg,''),
                   lnfile.ln_o3ins_dlproc,
                   lnfile.ln_insprem_dladj,
                   lnfile.ln_plamt,
                   lnfile.ln_mtd_prin,
                   lnfile.ln_mtd_lc,
                   lnfile.ln_mtd_int,
                   lnfile.ln_mtd_ot,
                   lnfile.ln_mtd_ex,
                   lnfile.ln_bnkrpt_chapter,
                   isnull(lnfile.ln_bmo_con_status,''),
                   isnull(lnfile.ln_bmo_rec_status,''),
                   lnfile.ln_app_number,
                   isnull(lnfile.ln_giftnet,''),
                   lnfile.ln_giftnet_our_po,
                   isnull(lnfile.ln_proc_disbursed,''),
                   isnull(lnfile.ln_nooddpy,''),
                   lnfile.ln_conprofile_1,
                   lnfile.ln_conprofile_2,
                   lnfile.ln_conprofile_3,
                   lnfile.ln_conprofile_4,
                   lnfile.ln_conprofile_5,
                   lnfile.ln_recprofile_1,
                   lnfile.ln_recprofile_2,
                   lnfile.ln_recprofile_3,
                   lnfile.ln_recprofile_4,
                   lnfile.ln_recprofile_5,
                   isnull(lnfile.ln_trw_status,''),
                   isnull(lnfile.ln_origin,''),
                   isnull(lnfile.ln_po_trcd,''),
                   lnfile.ln_nexis_crscore,
                   isnull(lnfile.ln_trw_b_consumer_ind,''),
                   isnull(lnfile.ln_parvheld_paid_fg,''),
                   isnull(lnfile.ln_acct_frozen,''),
                   isnull(lnfile.ln_trw_special_code,''),
                   isnull(lnfile.ln_creation_status,''),
                   lnfile.ln_totnolchg,
                   isnull(lnfile.ln_lbox,''),
                   lnfile.ln_extra_prin_lfp,
                   lnfile.ln_remain_int_since_lfp,
                   lnfile.ln_remain_prin_since_lfp,
                   isnull(lnfile.ln_interim_coupon,''),
                   lnfile.ln_dlcomm_o3_cmsi,
                   lnfile.ln_dlcomm_pp_cmsi,
                   isnull(lnfile.ln_trw_j_consumer_ind,''),
                   isnull(lnfile.ln_ach,''),
                   isnull(lnfile.ln_1099_c_fg,''),
                   isnull(lnfile.ln_trw_compliance_code,''),
                   isnull(lnfile.ln_unwind_reason_code,''),
                   isnull(lnfile.ln_pbi_pbd_fg,''),
                   isnull(lnfile.ln_old_acct_format_fg,''),
                   isnull(lnfile.ln_approved_by_user,''),
                   isnull(lnfile.ln_approved_by_code,''),
                   isnull(cast(lnfile.ln_approved_by_date
                                                  as varchar(10)),'0'),
                   lnfile.ln_approved_by_time,
                   isnull(cast(lnfile.ln_ltouch_date
                                                  as varchar(10)),'0'),
                   lnfile.ln_ltouch_time
            from dbo.lnfile
            where lnfile.ln_ownbr >= 10
              and lnfile.ln_ownbr <= 10
           end-exec.

           exec sql
               open ln1_cursor
           end-exec.

           perform sql-io-validation.

           if ( io-fg = 0 )
              move stat---good to ln1-cursor-stat.

      *-----------------------------------------------------------------
       read-ln1-file-next.
           move "read" to e-msg4.
           move 0 to io-fg.

           if ( ln1-cursor-stat-good )
              exec sql
                   fetch ln1_cursor into :qln-rec
              end-exec
           else
              move 9        to sqlcode
              move "rdnxcr" to sqlstate.

           perform sql-io-validation.

      *-----------------------------------------------------------------
       close-ln1-file.
           if ( ln1-cursor-stat-good )
              exec sql
                   close ln1_cursor
              end-exec
              move stat---bad to ln1-cursor-stat.

      *-----------------------------------------------------------------
       start-lp1-file.
           move "star" to e-msg4.
           move 0 to io-fg.

           if ( lp1-cursor-stat-good )
              perform close-lp1-file.

           exec sql
            DECLARE LP1_CURSOR CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   ISNULL(CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_USERID,''),
                   ISNULL(CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),'0'),
                   LPFILE.LP_POSTTIME,
                   ISNULL(CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_TRCD,''),
                   ISNULL(LPFILE.LP_REFNO,''),
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   ISNULL(CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   ISNULL(LPFILE.LP_IBPC,''),
                   ISNULL(LPFILE.LP_MANUAL_RB,''),
                   ISNULL(LPFILE.LP_INSTYPES,''),
                   ISNULL(LPFILE.LP_REV,''),
                   ISNULL(LPFILE.LP_REVTRCD,''),
                   LPFILE.LP_REVSEQNO,
                   ISNULL(LPFILE.LP_STATUSFG,''),
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_LCPAID,
                   ISNULL(CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   ISNULL(LPFILE.LP_COLLECTOR,''),
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   ISNULL(CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   ISNULL(LPFILE.LP_ALT_PREPAY_FG,'')
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO >= :QLN-OWNBR
              AND LPFILE.LP_BRNO <= :QLN-OWNBR
              AND LPFILE.LP_ACCTNO >= :QLN-ACCTNO
              AND LPFILE.LP_ACCTNO <= :QLN-ACCTNO
              AND LPFILE.LP_SEQNO >= 0
              AND LPFILE.LP_SEQNO <= 99999
            ORDER BY LPFILE.LP_BRNO,
                     LPFILE.LP_ACCTNO,
                     LPFILE.LP_SEQNO
           end-exec.

           exec sql
               open lp1_cursor
           end-exec.

           perform sql-io-validation.

           if ( io-fg = 0 )
              move stat---good to lp1-cursor-stat.

      *-----------------------------------------------------------------
       read-lp1-file-next.
           move "read" to e-msg4.
           move 0 to io-fg.

           if ( lp1-cursor-stat-good )
              exec sql
                   fetch lp1_cursor into :qlp-rec
              end-exec
           else
              move 9        to sqlcode
              move "rdnxcr" to sqlstate.

           perform sql-io-validation.

      *-----------------------------------------------------------------
       close-lp1-file.
           if ( lp1-cursor-stat-good )
              exec sql
                   close lp1_cursor
              end-exec
              move stat---bad to lp1-cursor-stat.

      *-----------------------------------------------------------------
       read-spa1-file.
           move "read" to e-msg4.
           move 0 to io-fg.

           EXEC SQL
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(CAST(SPAFILE.SPA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SPAFILE.SPA_DESC,''),
                   ISNULL(SPAFILE.SPA_NOTES,''),
                   ISNULL(CAST(SPAFILE.SPA_EFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SPAFILE.SPA_EXPDATE
                                                  AS VARCHAR(10)),'0'),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_EXCEPT,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_1,''),
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_2,''),
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_3,''),
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_4,''),
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_5,''),
                   ISNULL(SPAFILE.SPA_EXPR_LCHG,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   SPAFILE.SPA_JUDGRATE,
                   SPAFILE.SPA_JUDGYRTYPE,
                   ISNULL(SPAFILE.SPA_JUDGINTMETH,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   SPAFILE.SPA_ACC_YRTYPE,
                   ISNULL(SPAFILE.SPA_ACC_INTMETH,''),
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_5,
                   ISNULL(SPAFILE.SPA_1098FRMLA,''),
                   ISNULL(SPAFILE.SPA_DISFRMLA,''),
                   ISNULL(SPAFILE.SPA_CONTRFRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPAFILE.SPA_FINCHG,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   ISNULL(SPAFILE.SPA_RECPAYCD,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DISFRMLA_SPAY_FG,''),
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNPERCT_1,
                   ISNULL(SPAFILE.SPA_ERNPERCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_1,''),
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_TERM_1,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_1,''),
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNPERCT_2,
                   ISNULL(SPAFILE.SPA_ERNPERCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_2,''),
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_TERM_2,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_2,''),
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNPERCT_3,
                   ISNULL(SPAFILE.SPA_ERNPERCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_3,''),
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_TERM_3,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_3,''),
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNPERCT_4,
                   ISNULL(SPAFILE.SPA_ERNPERCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_4,''),
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_TERM_4,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_4,''),
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNPERCT_5,
                   ISNULL(SPAFILE.SPA_ERNPERCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_5,''),
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_TERM_5,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_5,''),
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNPERCT_6,
                   ISNULL(SPAFILE.SPA_ERNPERCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_6,''),
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_TERM_6,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_6,''),
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNPERCT_7,
                   ISNULL(SPAFILE.SPA_ERNPERCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_7,''),
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_TERM_7,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_7,''),
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNPERCT_8,
                   ISNULL(SPAFILE.SPA_ERNPERCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_8,''),
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_TERM_8,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_8,''),
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNPERCT_9,
                   ISNULL(SPAFILE.SPA_ERNPERCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_9,''),
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_TERM_9,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_9,''),
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_10,
                   SPAFILE.SPA_ERNPERCT_10,
                   ISNULL(SPAFILE.SPA_ERNPERCD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_10,''),
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_10,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCEL_CD,''),
                   ISNULL(SPAFILE.SPA_ERNJD_CD,''),
                   ISNULL(SPAFILE.SPA_MIDSTRERNCD,''),
                   SPAFILE.SPA_EARN_ORDER_1,
                   SPAFILE.SPA_EARN_ORDER_2,
                   SPAFILE.SPA_EARN_ORDER_3,
                   SPAFILE.SPA_EARN_ORDER_4,
                   SPAFILE.SPA_EARN_ORDER_5,
                   SPAFILE.SPA_EARN_ORDER_6,
                   SPAFILE.SPA_EARN_ORDER_7,
                   SPAFILE.SPA_EARN_ORDER_8,
                   SPAFILE.SPA_EARN_ORDER_9,
                   SPAFILE.SPA_EARN_ORDER_10,
                   ISNULL(SPAFILE.SPA_ERNDELQFRMLA,''),
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   ISNULL(SPAFILE.SPA_1098_INC_LC,''),
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   SPAFILE.SPA_LCGRACE,
                   ISNULL(SPAFILE.SPA_LCGRACECD,''),
                   ISNULL(SPAFILE.SPA_LCTYPE,''),
                   SPAFILE.SPA_LCRATE,
                   ISNULL(SPAFILE.SPA_LCFRMLA,''),
                   SPAFILE.SPA_LCMINCHG,
                   SPAFILE.SPA_LCMAXAMT,
                   SPAFILE.SPA_LCDELFAC,
                   SPAFILE.SPA_LCYRTYPE,
                   SPAFILE.SPA_LC_CODE,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   SPAFILE.SPA_DEFFRMLA,
                   ISNULL(SPAFILE.SPA_DEFEXCEPT,''),
                   SPAFILE.SPA_DEFWORKER,
                   SPAFILE.SPA_DEFPERCD,
                   SPAFILE.SPA_DEFBASE,
                   ISNULL(SPAFILE.SPA_DEFPOLCD,''),
                   SPAFILE.SPA_DEFPOLMX_1,
                   SPAFILE.SPA_DEFPOLMX_2,
                   SPAFILE.SPA_DEFPOLMX_3,
                   SPAFILE.SPA_DEFMIN,
                   SPAFILE.SPA_DEFMAX,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPAFILE.SPA_PRFORM_1,
                   ISNULL(SPAFILE.SPA_PRFLAG_1,''),
                   SPAFILE.SPA_PRFORM_2,
                   ISNULL(SPAFILE.SPA_PRFLAG_2,''),
                   SPAFILE.SPA_PRFORM_3,
                   ISNULL(SPAFILE.SPA_PRFLAG_3,''),
                   SPAFILE.SPA_PRFORM_4,
                   ISNULL(SPAFILE.SPA_PRFLAG_4,''),
                   SPAFILE.SPA_PRFORM_5,
                   ISNULL(SPAFILE.SPA_PRFLAG_5,''),
                   SPAFILE.SPA_PRFORM_6,
                   ISNULL(SPAFILE.SPA_PRFLAG_6,''),
                   SPAFILE.SPA_PRFORM_7,
                   ISNULL(SPAFILE.SPA_PRFLAG_7,''),
                   SPAFILE.SPA_PRFORM_8,
                   ISNULL(SPAFILE.SPA_PRFLAG_8,''),
                   SPAFILE.SPA_PRFORM_9,
                   ISNULL(SPAFILE.SPA_PRFLAG_9,''),
                   SPAFILE.SPA_PRFORM_10,
                   ISNULL(SPAFILE.SPA_PRFLAG_10,''),
                   SPAFILE.SPA_PRFORM_11,
                   ISNULL(SPAFILE.SPA_PRFLAG_11,''),
                   SPAFILE.SPA_PRFORM_12,
                   ISNULL(SPAFILE.SPA_PRFLAG_12,''),
                   SPAFILE.SPA_PRFORM_13,
                   ISNULL(SPAFILE.SPA_PRFLAG_13,''),
                   SPAFILE.SPA_PRFORM_14,
                   ISNULL(SPAFILE.SPA_PRFLAG_14,''),
                   SPAFILE.SPA_PRFORM_15,
                   ISNULL(SPAFILE.SPA_PRFLAG_15,''),
                   SPAFILE.SPA_PRFORM_16,
                   ISNULL(SPAFILE.SPA_PRFLAG_16,''),
                   SPAFILE.SPA_PRFORM_17,
                   ISNULL(SPAFILE.SPA_PRFLAG_17,''),
                   SPAFILE.SPA_PRFORM_18,
                   ISNULL(SPAFILE.SPA_PRFLAG_18,''),
                   SPAFILE.SPA_PRFORM_19,
                   ISNULL(SPAFILE.SPA_PRFLAG_19,''),
                   SPAFILE.SPA_PRFORM_20,
                   ISNULL(SPAFILE.SPA_PRFLAG_20,''),
                   ISNULL(SPAFILE.SPA_IMED_REC,''),
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_SCRATE,
                   SPAFILE.SPA_SCMIN,
                   SPAFILE.SPA_SCMAX,
                   SPAFILE.SPA_MFFRMLA,
                   SPAFILE.SPA_MFRATE,
                   SPAFILE.SPA_MFMIN,
                   SPAFILE.SPA_MFMAX,
                   ISNULL(SPAFILE.SPA_INTMETHOD,''),
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   ISNULL(SPAFILE.SPA_PYEXFMDATE,''),
                   ISNULL(SPAFILE.SPA_PYEXBASECD,''),
                   SPAFILE.SPA_PYEXBASE,
                   SPAFILE.SPA_PYEXDAYS,
                   SPAFILE.SPA_CAL_SPFRMLA,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_TBFRMLA_1,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,''),
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_TBFRMLA_2,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_2_5,
                   SPAFILE.SPA_MINSF_FRMLA,
                   SPAFILE.SPA_MINSF_CHG,
                   SPAFILE.SPA_MAXAPR_FRMLA,
                   SPAFILE.SPA_EDT_NOEDITS_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_1,''),
                   SPAFILE.SPA_EDT_RATE_1_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_1,''),
                   SPAFILE.SPA_EDT_AMT_1_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_1,''),
                   SPAFILE.SPA_EDT_TERM_1_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_2,''),
                   SPAFILE.SPA_EDT_RATE_1_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_2,''),
                   SPAFILE.SPA_EDT_AMT_1_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_2,''),
                   SPAFILE.SPA_EDT_TERM_1_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_3,''),
                   SPAFILE.SPA_EDT_RATE_1_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_3,''),
                   SPAFILE.SPA_EDT_AMT_1_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_3,''),
                   SPAFILE.SPA_EDT_TERM_1_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_4,''),
                   SPAFILE.SPA_EDT_RATE_1_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_4,''),
                   SPAFILE.SPA_EDT_AMT_1_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_4,''),
                   SPAFILE.SPA_EDT_TERM_1_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_5,''),
                   SPAFILE.SPA_EDT_RATE_1_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_5,''),
                   SPAFILE.SPA_EDT_AMT_1_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_5,''),
                   SPAFILE.SPA_EDT_TERM_1_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_6,''),
                   SPAFILE.SPA_EDT_RATE_1_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_6,''),
                   SPAFILE.SPA_EDT_AMT_1_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_6,''),
                   SPAFILE.SPA_EDT_TERM_1_6,
                   SPAFILE.SPA_EDT_NOEDITS_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_1,''),
                   SPAFILE.SPA_EDT_RATE_2_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_1,''),
                   SPAFILE.SPA_EDT_AMT_2_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_1,''),
                   SPAFILE.SPA_EDT_TERM_2_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_2,''),
                   SPAFILE.SPA_EDT_RATE_2_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_2,''),
                   SPAFILE.SPA_EDT_AMT_2_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_2,''),
                   SPAFILE.SPA_EDT_TERM_2_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_3,''),
                   SPAFILE.SPA_EDT_RATE_2_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_3,''),
                   SPAFILE.SPA_EDT_AMT_2_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_3,''),
                   SPAFILE.SPA_EDT_TERM_2_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_4,''),
                   SPAFILE.SPA_EDT_RATE_2_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_4,''),
                   SPAFILE.SPA_EDT_AMT_2_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_4,''),
                   SPAFILE.SPA_EDT_TERM_2_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_5,''),
                   SPAFILE.SPA_EDT_RATE_2_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_5,''),
                   SPAFILE.SPA_EDT_AMT_2_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_5,''),
                   SPAFILE.SPA_EDT_TERM_2_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_6,''),
                   SPAFILE.SPA_EDT_RATE_2_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_6,''),
                   SPAFILE.SPA_EDT_AMT_2_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_6,''),
                   SPAFILE.SPA_EDT_TERM_2_6
            INTO :QSPA-REC
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST = :QSPA-ORGST
              AND SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
              AND SPAFILE.SPA_LAWCODE = :QSPA-LAWCODE 
           END-EXEC.

           perform sql-io-validation.

      *-----------------------------------------------------------------
       read-spb1-file.
           move "read" to e-msg4.
           move 0 to io-fg.

           EXEC SQL
            SELECT ISNULL(SPBFILE.SPB_ORGST,''),
                   SPBFILE.SPB_SPRCLASS,
                   SPBFILE.SPB_SUBCLASS,
                   SPBFILE.SPB_LAWCODE,
                   SPBFILE.SPB_INSCOM_1,
                   SPBFILE.SPB_INSRATECD_1,
                   SPBFILE.SPB_INSJFAC_1,
                   SPBFILE.SPB_INSDISCNT_1,
                   ISNULL(SPBFILE.SPB_INSCHGABL_1,''),
                   SPBFILE.SPB_INSTERM_1_1,
                   SPBFILE.SPB_INSRATE_1_1,
                   SPBFILE.SPB_INSTERM_1_2,
                   SPBFILE.SPB_INSRATE_1_2,
                   SPBFILE.SPB_INSTERM_1_3,
                   SPBFILE.SPB_INSRATE_1_3,
                   SPBFILE.SPB_INSTERM_1_4,
                   SPBFILE.SPB_INSRATE_1_4,
                   SPBFILE.SPB_INSTERM_1_5,
                   SPBFILE.SPB_INSRATE_1_5,
                   SPBFILE.SPB_INSTERM_1_6,
                   SPBFILE.SPB_INSRATE_1_6,
                   SPBFILE.SPB_INSTERM_1_7,
                   SPBFILE.SPB_INSRATE_1_7,
                   SPBFILE.SPB_INSTERM_1_8,
                   SPBFILE.SPB_INSRATE_1_8,
                   SPBFILE.SPB_INSTERM_1_9,
                   SPBFILE.SPB_INSRATE_1_9,
                   SPBFILE.SPB_INSTERM_1_10,
                   SPBFILE.SPB_INSRATE_1_10,
                   SPBFILE.SPB_INSTERM_1_11,
                   SPBFILE.SPB_INSRATE_1_11,
                   SPBFILE.SPB_INSTERM_1_12,
                   SPBFILE.SPB_INSRATE_1_12,
                   SPBFILE.SPB_INSTERM_1_13,
                   SPBFILE.SPB_INSRATE_1_13,
                   SPBFILE.SPB_INSTERM_1_14,
                   SPBFILE.SPB_INSRATE_1_14,
                   SPBFILE.SPB_INSTERM_1_15,
                   SPBFILE.SPB_INSRATE_1_15,
                   SPBFILE.SPB_INSTERM_1_16,
                   SPBFILE.SPB_INSRATE_1_16,
                   SPBFILE.SPB_INSTERM_1_17,
                   SPBFILE.SPB_INSRATE_1_17,
                   SPBFILE.SPB_INSTERM_1_18,
                   SPBFILE.SPB_INSRATE_1_18,
                   SPBFILE.SPB_INSTERM_1_19,
                   SPBFILE.SPB_INSRATE_1_19,
                   SPBFILE.SPB_INSTERM_1_20,
                   SPBFILE.SPB_INSRATE_1_20,
                   SPBFILE.SPB_INSMINPREM_1,
                   SPBFILE.SPB_INSMINCHGABL_1,
                   SPBFILE.SPB_INSMAXCHGABL_1,
                   SPBFILE.SPB_INSMAXBENIF_1,
                   SPBFILE.SPB_INSMAXTERM_1,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_1,''),
                   SPBFILE.SPB_INSEXCEPT_1,
                   SPBFILE.SPB_INSNOVER_1,
                   SPBFILE.SPB_INSMINAGE_1_1,
                   SPBFILE.SPB_INSMAXAGE_1_1,
                   SPBFILE.SPB_INSMAXCOV_1_1,
                   SPBFILE.SPB_INSMAXTRM_1_1,
                   SPBFILE.SPB_INSMINAGE_1_2,
                   SPBFILE.SPB_INSMAXAGE_1_2,
                   SPBFILE.SPB_INSMAXCOV_1_2,
                   SPBFILE.SPB_INSMAXTRM_1_2,
                   SPBFILE.SPB_INSMINAGE_1_3,
                   SPBFILE.SPB_INSMAXAGE_1_3,
                   SPBFILE.SPB_INSMAXCOV_1_3,
                   SPBFILE.SPB_INSMAXTRM_1_3,
                   SPBFILE.SPB_INSMINAGE_1_4,
                   SPBFILE.SPB_INSMAXAGE_1_4,
                   SPBFILE.SPB_INSMAXCOV_1_4,
                   SPBFILE.SPB_INSMAXTRM_1_4,
                   SPBFILE.SPB_INSMINAGE_1_5,
                   SPBFILE.SPB_INSMAXAGE_1_5,
                   SPBFILE.SPB_INSMAXCOV_1_5,
                   SPBFILE.SPB_INSMAXTRM_1_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_1,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_1,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_1,''),
                   SPBFILE.SPB_INSCOM_2,
                   SPBFILE.SPB_INSRATECD_2,
                   SPBFILE.SPB_INSJFAC_2,
                   SPBFILE.SPB_INSDISCNT_2,
                   ISNULL(SPBFILE.SPB_INSCHGABL_2,''),
                   SPBFILE.SPB_INSTERM_2_1,
                   SPBFILE.SPB_INSRATE_2_1,
                   SPBFILE.SPB_INSTERM_2_2,
                   SPBFILE.SPB_INSRATE_2_2,
                   SPBFILE.SPB_INSTERM_2_3,
                   SPBFILE.SPB_INSRATE_2_3,
                   SPBFILE.SPB_INSTERM_2_4,
                   SPBFILE.SPB_INSRATE_2_4,
                   SPBFILE.SPB_INSTERM_2_5,
                   SPBFILE.SPB_INSRATE_2_5,
                   SPBFILE.SPB_INSTERM_2_6,
                   SPBFILE.SPB_INSRATE_2_6,
                   SPBFILE.SPB_INSTERM_2_7,
                   SPBFILE.SPB_INSRATE_2_7,
                   SPBFILE.SPB_INSTERM_2_8,
                   SPBFILE.SPB_INSRATE_2_8,
                   SPBFILE.SPB_INSTERM_2_9,
                   SPBFILE.SPB_INSRATE_2_9,
                   SPBFILE.SPB_INSTERM_2_10,
                   SPBFILE.SPB_INSRATE_2_10,
                   SPBFILE.SPB_INSTERM_2_11,
                   SPBFILE.SPB_INSRATE_2_11,
                   SPBFILE.SPB_INSTERM_2_12,
                   SPBFILE.SPB_INSRATE_2_12,
                   SPBFILE.SPB_INSTERM_2_13,
                   SPBFILE.SPB_INSRATE_2_13,
                   SPBFILE.SPB_INSTERM_2_14,
                   SPBFILE.SPB_INSRATE_2_14,
                   SPBFILE.SPB_INSTERM_2_15,
                   SPBFILE.SPB_INSRATE_2_15,
                   SPBFILE.SPB_INSTERM_2_16,
                   SPBFILE.SPB_INSRATE_2_16,
                   SPBFILE.SPB_INSTERM_2_17,
                   SPBFILE.SPB_INSRATE_2_17,
                   SPBFILE.SPB_INSTERM_2_18,
                   SPBFILE.SPB_INSRATE_2_18,
                   SPBFILE.SPB_INSTERM_2_19,
                   SPBFILE.SPB_INSRATE_2_19,
                   SPBFILE.SPB_INSTERM_2_20,
                   SPBFILE.SPB_INSRATE_2_20,
                   SPBFILE.SPB_INSMINPREM_2,
                   SPBFILE.SPB_INSMINCHGABL_2,
                   SPBFILE.SPB_INSMAXCHGABL_2,
                   SPBFILE.SPB_INSMAXBENIF_2,
                   SPBFILE.SPB_INSMAXTERM_2,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_2,''),
                   SPBFILE.SPB_INSEXCEPT_2,
                   SPBFILE.SPB_INSNOVER_2,
                   SPBFILE.SPB_INSMINAGE_2_1,
                   SPBFILE.SPB_INSMAXAGE_2_1,
                   SPBFILE.SPB_INSMAXCOV_2_1,
                   SPBFILE.SPB_INSMAXTRM_2_1,
                   SPBFILE.SPB_INSMINAGE_2_2,
                   SPBFILE.SPB_INSMAXAGE_2_2,
                   SPBFILE.SPB_INSMAXCOV_2_2,
                   SPBFILE.SPB_INSMAXTRM_2_2,
                   SPBFILE.SPB_INSMINAGE_2_3,
                   SPBFILE.SPB_INSMAXAGE_2_3,
                   SPBFILE.SPB_INSMAXCOV_2_3,
                   SPBFILE.SPB_INSMAXTRM_2_3,
                   SPBFILE.SPB_INSMINAGE_2_4,
                   SPBFILE.SPB_INSMAXAGE_2_4,
                   SPBFILE.SPB_INSMAXCOV_2_4,
                   SPBFILE.SPB_INSMAXTRM_2_4,
                   SPBFILE.SPB_INSMINAGE_2_5,
                   SPBFILE.SPB_INSMAXAGE_2_5,
                   SPBFILE.SPB_INSMAXCOV_2_5,
                   SPBFILE.SPB_INSMAXTRM_2_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_2,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_2,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_2,''),
                   SPBFILE.SPB_INSCOM_3,
                   SPBFILE.SPB_INSRATECD_3,
                   SPBFILE.SPB_INSJFAC_3,
                   SPBFILE.SPB_INSDISCNT_3,
                   ISNULL(SPBFILE.SPB_INSCHGABL_3,''),
                   SPBFILE.SPB_INSTERM_3_1,
                   SPBFILE.SPB_INSRATE_3_1,
                   SPBFILE.SPB_INSTERM_3_2,
                   SPBFILE.SPB_INSRATE_3_2,
                   SPBFILE.SPB_INSTERM_3_3,
                   SPBFILE.SPB_INSRATE_3_3,
                   SPBFILE.SPB_INSTERM_3_4,
                   SPBFILE.SPB_INSRATE_3_4,
                   SPBFILE.SPB_INSTERM_3_5,
                   SPBFILE.SPB_INSRATE_3_5,
                   SPBFILE.SPB_INSTERM_3_6,
                   SPBFILE.SPB_INSRATE_3_6,
                   SPBFILE.SPB_INSTERM_3_7,
                   SPBFILE.SPB_INSRATE_3_7,
                   SPBFILE.SPB_INSTERM_3_8,
                   SPBFILE.SPB_INSRATE_3_8,
                   SPBFILE.SPB_INSTERM_3_9,
                   SPBFILE.SPB_INSRATE_3_9,
                   SPBFILE.SPB_INSTERM_3_10,
                   SPBFILE.SPB_INSRATE_3_10,
                   SPBFILE.SPB_INSTERM_3_11,
                   SPBFILE.SPB_INSRATE_3_11,
                   SPBFILE.SPB_INSTERM_3_12,
                   SPBFILE.SPB_INSRATE_3_12,
                   SPBFILE.SPB_INSTERM_3_13,
                   SPBFILE.SPB_INSRATE_3_13,
                   SPBFILE.SPB_INSTERM_3_14,
                   SPBFILE.SPB_INSRATE_3_14,
                   SPBFILE.SPB_INSTERM_3_15,
                   SPBFILE.SPB_INSRATE_3_15,
                   SPBFILE.SPB_INSTERM_3_16,
                   SPBFILE.SPB_INSRATE_3_16,
                   SPBFILE.SPB_INSTERM_3_17,
                   SPBFILE.SPB_INSRATE_3_17,
                   SPBFILE.SPB_INSTERM_3_18,
                   SPBFILE.SPB_INSRATE_3_18,
                   SPBFILE.SPB_INSTERM_3_19,
                   SPBFILE.SPB_INSRATE_3_19,
                   SPBFILE.SPB_INSTERM_3_20,
                   SPBFILE.SPB_INSRATE_3_20,
                   SPBFILE.SPB_INSMINPREM_3,
                   SPBFILE.SPB_INSMINCHGABL_3,
                   SPBFILE.SPB_INSMAXCHGABL_3,
                   SPBFILE.SPB_INSMAXBENIF_3,
                   SPBFILE.SPB_INSMAXTERM_3,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_3,''),
                   SPBFILE.SPB_INSEXCEPT_3,
                   SPBFILE.SPB_INSNOVER_3,
                   SPBFILE.SPB_INSMINAGE_3_1,
                   SPBFILE.SPB_INSMAXAGE_3_1,
                   SPBFILE.SPB_INSMAXCOV_3_1,
                   SPBFILE.SPB_INSMAXTRM_3_1,
                   SPBFILE.SPB_INSMINAGE_3_2,
                   SPBFILE.SPB_INSMAXAGE_3_2,
                   SPBFILE.SPB_INSMAXCOV_3_2,
                   SPBFILE.SPB_INSMAXTRM_3_2,
                   SPBFILE.SPB_INSMINAGE_3_3,
                   SPBFILE.SPB_INSMAXAGE_3_3,
                   SPBFILE.SPB_INSMAXCOV_3_3,
                   SPBFILE.SPB_INSMAXTRM_3_3,
                   SPBFILE.SPB_INSMINAGE_3_4,
                   SPBFILE.SPB_INSMAXAGE_3_4,
                   SPBFILE.SPB_INSMAXCOV_3_4,
                   SPBFILE.SPB_INSMAXTRM_3_4,
                   SPBFILE.SPB_INSMINAGE_3_5,
                   SPBFILE.SPB_INSMAXAGE_3_5,
                   SPBFILE.SPB_INSMAXCOV_3_5,
                   SPBFILE.SPB_INSMAXTRM_3_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_3,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_3,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_3,''),
                   SPBFILE.SPB_INSCOM_4,
                   SPBFILE.SPB_INSRATECD_4,
                   SPBFILE.SPB_INSJFAC_4,
                   SPBFILE.SPB_INSDISCNT_4,
                   ISNULL(SPBFILE.SPB_INSCHGABL_4,''),
                   SPBFILE.SPB_INSTERM_4_1,
                   SPBFILE.SPB_INSRATE_4_1,
                   SPBFILE.SPB_INSTERM_4_2,
                   SPBFILE.SPB_INSRATE_4_2,
                   SPBFILE.SPB_INSTERM_4_3,
                   SPBFILE.SPB_INSRATE_4_3,
                   SPBFILE.SPB_INSTERM_4_4,
                   SPBFILE.SPB_INSRATE_4_4,
                   SPBFILE.SPB_INSTERM_4_5,
                   SPBFILE.SPB_INSRATE_4_5,
                   SPBFILE.SPB_INSTERM_4_6,
                   SPBFILE.SPB_INSRATE_4_6,
                   SPBFILE.SPB_INSTERM_4_7,
                   SPBFILE.SPB_INSRATE_4_7,
                   SPBFILE.SPB_INSTERM_4_8,
                   SPBFILE.SPB_INSRATE_4_8,
                   SPBFILE.SPB_INSTERM_4_9,
                   SPBFILE.SPB_INSRATE_4_9,
                   SPBFILE.SPB_INSTERM_4_10,
                   SPBFILE.SPB_INSRATE_4_10,
                   SPBFILE.SPB_INSTERM_4_11,
                   SPBFILE.SPB_INSRATE_4_11,
                   SPBFILE.SPB_INSTERM_4_12,
                   SPBFILE.SPB_INSRATE_4_12,
                   SPBFILE.SPB_INSTERM_4_13,
                   SPBFILE.SPB_INSRATE_4_13,
                   SPBFILE.SPB_INSTERM_4_14,
                   SPBFILE.SPB_INSRATE_4_14,
                   SPBFILE.SPB_INSTERM_4_15,
                   SPBFILE.SPB_INSRATE_4_15,
                   SPBFILE.SPB_INSTERM_4_16,
                   SPBFILE.SPB_INSRATE_4_16,
                   SPBFILE.SPB_INSTERM_4_17,
                   SPBFILE.SPB_INSRATE_4_17,
                   SPBFILE.SPB_INSTERM_4_18,
                   SPBFILE.SPB_INSRATE_4_18,
                   SPBFILE.SPB_INSTERM_4_19,
                   SPBFILE.SPB_INSRATE_4_19,
                   SPBFILE.SPB_INSTERM_4_20,
                   SPBFILE.SPB_INSRATE_4_20,
                   SPBFILE.SPB_INSMINPREM_4,
                   SPBFILE.SPB_INSMINCHGABL_4,
                   SPBFILE.SPB_INSMAXCHGABL_4,
                   SPBFILE.SPB_INSMAXBENIF_4,
                   SPBFILE.SPB_INSMAXTERM_4,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_4,''),
                   SPBFILE.SPB_INSEXCEPT_4,
                   SPBFILE.SPB_INSNOVER_4,
                   SPBFILE.SPB_INSMINAGE_4_1,
                   SPBFILE.SPB_INSMAXAGE_4_1,
                   SPBFILE.SPB_INSMAXCOV_4_1,
                   SPBFILE.SPB_INSMAXTRM_4_1,
                   SPBFILE.SPB_INSMINAGE_4_2,
                   SPBFILE.SPB_INSMAXAGE_4_2,
                   SPBFILE.SPB_INSMAXCOV_4_2,
                   SPBFILE.SPB_INSMAXTRM_4_2,
                   SPBFILE.SPB_INSMINAGE_4_3,
                   SPBFILE.SPB_INSMAXAGE_4_3,
                   SPBFILE.SPB_INSMAXCOV_4_3,
                   SPBFILE.SPB_INSMAXTRM_4_3,
                   SPBFILE.SPB_INSMINAGE_4_4,
                   SPBFILE.SPB_INSMAXAGE_4_4,
                   SPBFILE.SPB_INSMAXCOV_4_4,
                   SPBFILE.SPB_INSMAXTRM_4_4,
                   SPBFILE.SPB_INSMINAGE_4_5,
                   SPBFILE.SPB_INSMAXAGE_4_5,
                   SPBFILE.SPB_INSMAXCOV_4_5,
                   SPBFILE.SPB_INSMAXTRM_4_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_4,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_4,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_4,''),
                   SPBFILE.SPB_INSCOM_5,
                   SPBFILE.SPB_INSRATECD_5,
                   SPBFILE.SPB_INSJFAC_5,
                   SPBFILE.SPB_INSDISCNT_5,
                   ISNULL(SPBFILE.SPB_INSCHGABL_5,''),
                   SPBFILE.SPB_INSTERM_5_1,
                   SPBFILE.SPB_INSRATE_5_1,
                   SPBFILE.SPB_INSTERM_5_2,
                   SPBFILE.SPB_INSRATE_5_2,
                   SPBFILE.SPB_INSTERM_5_3,
                   SPBFILE.SPB_INSRATE_5_3,
                   SPBFILE.SPB_INSTERM_5_4,
                   SPBFILE.SPB_INSRATE_5_4,
                   SPBFILE.SPB_INSTERM_5_5,
                   SPBFILE.SPB_INSRATE_5_5,
                   SPBFILE.SPB_INSTERM_5_6,
                   SPBFILE.SPB_INSRATE_5_6,
                   SPBFILE.SPB_INSTERM_5_7,
                   SPBFILE.SPB_INSRATE_5_7,
                   SPBFILE.SPB_INSTERM_5_8,
                   SPBFILE.SPB_INSRATE_5_8,
                   SPBFILE.SPB_INSTERM_5_9,
                   SPBFILE.SPB_INSRATE_5_9,
                   SPBFILE.SPB_INSTERM_5_10,
                   SPBFILE.SPB_INSRATE_5_10,
                   SPBFILE.SPB_INSTERM_5_11,
                   SPBFILE.SPB_INSRATE_5_11,
                   SPBFILE.SPB_INSTERM_5_12,
                   SPBFILE.SPB_INSRATE_5_12,
                   SPBFILE.SPB_INSTERM_5_13,
                   SPBFILE.SPB_INSRATE_5_13,
                   SPBFILE.SPB_INSTERM_5_14,
                   SPBFILE.SPB_INSRATE_5_14,
                   SPBFILE.SPB_INSTERM_5_15,
                   SPBFILE.SPB_INSRATE_5_15,
                   SPBFILE.SPB_INSTERM_5_16,
                   SPBFILE.SPB_INSRATE_5_16,
                   SPBFILE.SPB_INSTERM_5_17,
                   SPBFILE.SPB_INSRATE_5_17,
                   SPBFILE.SPB_INSTERM_5_18,
                   SPBFILE.SPB_INSRATE_5_18,
                   SPBFILE.SPB_INSTERM_5_19,
                   SPBFILE.SPB_INSRATE_5_19,
                   SPBFILE.SPB_INSTERM_5_20,
                   SPBFILE.SPB_INSRATE_5_20,
                   SPBFILE.SPB_INSMINPREM_5,
                   SPBFILE.SPB_INSMINCHGABL_5,
                   SPBFILE.SPB_INSMAXCHGABL_5,
                   SPBFILE.SPB_INSMAXBENIF_5,
                   SPBFILE.SPB_INSMAXTERM_5,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_5,''),
                   SPBFILE.SPB_INSEXCEPT_5,
                   SPBFILE.SPB_INSNOVER_5,
                   SPBFILE.SPB_INSMINAGE_5_1,
                   SPBFILE.SPB_INSMAXAGE_5_1,
                   SPBFILE.SPB_INSMAXCOV_5_1,
                   SPBFILE.SPB_INSMAXTRM_5_1,
                   SPBFILE.SPB_INSMINAGE_5_2,
                   SPBFILE.SPB_INSMAXAGE_5_2,
                   SPBFILE.SPB_INSMAXCOV_5_2,
                   SPBFILE.SPB_INSMAXTRM_5_2,
                   SPBFILE.SPB_INSMINAGE_5_3,
                   SPBFILE.SPB_INSMAXAGE_5_3,
                   SPBFILE.SPB_INSMAXCOV_5_3,
                   SPBFILE.SPB_INSMAXTRM_5_3,
                   SPBFILE.SPB_INSMINAGE_5_4,
                   SPBFILE.SPB_INSMAXAGE_5_4,
                   SPBFILE.SPB_INSMAXCOV_5_4,
                   SPBFILE.SPB_INSMAXTRM_5_4,
                   SPBFILE.SPB_INSMINAGE_5_5,
                   SPBFILE.SPB_INSMAXAGE_5_5,
                   SPBFILE.SPB_INSMAXCOV_5_5,
                   SPBFILE.SPB_INSMAXTRM_5_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_5,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_5,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_5,''),
                   SPBFILE.SPB_INSCOM_6,
                   SPBFILE.SPB_INSRATECD_6,
                   SPBFILE.SPB_INSJFAC_6,
                   SPBFILE.SPB_INSDISCNT_6,
                   ISNULL(SPBFILE.SPB_INSCHGABL_6,''),
                   SPBFILE.SPB_INSTERM_6_1,
                   SPBFILE.SPB_INSRATE_6_1,
                   SPBFILE.SPB_INSTERM_6_2,
                   SPBFILE.SPB_INSRATE_6_2,
                   SPBFILE.SPB_INSTERM_6_3,
                   SPBFILE.SPB_INSRATE_6_3,
                   SPBFILE.SPB_INSTERM_6_4,
                   SPBFILE.SPB_INSRATE_6_4,
                   SPBFILE.SPB_INSTERM_6_5,
                   SPBFILE.SPB_INSRATE_6_5,
                   SPBFILE.SPB_INSTERM_6_6,
                   SPBFILE.SPB_INSRATE_6_6,
                   SPBFILE.SPB_INSTERM_6_7,
                   SPBFILE.SPB_INSRATE_6_7,
                   SPBFILE.SPB_INSTERM_6_8,
                   SPBFILE.SPB_INSRATE_6_8,
                   SPBFILE.SPB_INSTERM_6_9,
                   SPBFILE.SPB_INSRATE_6_9,
                   SPBFILE.SPB_INSTERM_6_10,
                   SPBFILE.SPB_INSRATE_6_10,
                   SPBFILE.SPB_INSTERM_6_11,
                   SPBFILE.SPB_INSRATE_6_11,
                   SPBFILE.SPB_INSTERM_6_12,
                   SPBFILE.SPB_INSRATE_6_12,
                   SPBFILE.SPB_INSTERM_6_13,
                   SPBFILE.SPB_INSRATE_6_13,
                   SPBFILE.SPB_INSTERM_6_14,
                   SPBFILE.SPB_INSRATE_6_14,
                   SPBFILE.SPB_INSTERM_6_15,
                   SPBFILE.SPB_INSRATE_6_15,
                   SPBFILE.SPB_INSTERM_6_16,
                   SPBFILE.SPB_INSRATE_6_16,
                   SPBFILE.SPB_INSTERM_6_17,
                   SPBFILE.SPB_INSRATE_6_17,
                   SPBFILE.SPB_INSTERM_6_18,
                   SPBFILE.SPB_INSRATE_6_18,
                   SPBFILE.SPB_INSTERM_6_19,
                   SPBFILE.SPB_INSRATE_6_19,
                   SPBFILE.SPB_INSTERM_6_20,
                   SPBFILE.SPB_INSRATE_6_20,
                   SPBFILE.SPB_INSMINPREM_6,
                   SPBFILE.SPB_INSMINCHGABL_6,
                   SPBFILE.SPB_INSMAXCHGABL_6,
                   SPBFILE.SPB_INSMAXBENIF_6,
                   SPBFILE.SPB_INSMAXTERM_6,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_6,''),
                   SPBFILE.SPB_INSEXCEPT_6,
                   SPBFILE.SPB_INSNOVER_6,
                   SPBFILE.SPB_INSMINAGE_6_1,
                   SPBFILE.SPB_INSMAXAGE_6_1,
                   SPBFILE.SPB_INSMAXCOV_6_1,
                   SPBFILE.SPB_INSMAXTRM_6_1,
                   SPBFILE.SPB_INSMINAGE_6_2,
                   SPBFILE.SPB_INSMAXAGE_6_2,
                   SPBFILE.SPB_INSMAXCOV_6_2,
                   SPBFILE.SPB_INSMAXTRM_6_2,
                   SPBFILE.SPB_INSMINAGE_6_3,
                   SPBFILE.SPB_INSMAXAGE_6_3,
                   SPBFILE.SPB_INSMAXCOV_6_3,
                   SPBFILE.SPB_INSMAXTRM_6_3,
                   SPBFILE.SPB_INSMINAGE_6_4,
                   SPBFILE.SPB_INSMAXAGE_6_4,
                   SPBFILE.SPB_INSMAXCOV_6_4,
                   SPBFILE.SPB_INSMAXTRM_6_4,
                   SPBFILE.SPB_INSMINAGE_6_5,
                   SPBFILE.SPB_INSMAXAGE_6_5,
                   SPBFILE.SPB_INSMAXCOV_6_5,
                   SPBFILE.SPB_INSMAXTRM_6_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_6,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_6,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_6,''),
                   SPBFILE.SPB_INSCOM_7,
                   SPBFILE.SPB_INSRATECD_7,
                   SPBFILE.SPB_INSJFAC_7,
                   SPBFILE.SPB_INSDISCNT_7,
                   ISNULL(SPBFILE.SPB_INSCHGABL_7,''),
                   SPBFILE.SPB_INSTERM_7_1,
                   SPBFILE.SPB_INSRATE_7_1,
                   SPBFILE.SPB_INSTERM_7_2,
                   SPBFILE.SPB_INSRATE_7_2,
                   SPBFILE.SPB_INSTERM_7_3,
                   SPBFILE.SPB_INSRATE_7_3,
                   SPBFILE.SPB_INSTERM_7_4,
                   SPBFILE.SPB_INSRATE_7_4,
                   SPBFILE.SPB_INSTERM_7_5,
                   SPBFILE.SPB_INSRATE_7_5,
                   SPBFILE.SPB_INSTERM_7_6,
                   SPBFILE.SPB_INSRATE_7_6,
                   SPBFILE.SPB_INSTERM_7_7,
                   SPBFILE.SPB_INSRATE_7_7,
                   SPBFILE.SPB_INSTERM_7_8,
                   SPBFILE.SPB_INSRATE_7_8,
                   SPBFILE.SPB_INSTERM_7_9,
                   SPBFILE.SPB_INSRATE_7_9,
                   SPBFILE.SPB_INSTERM_7_10,
                   SPBFILE.SPB_INSRATE_7_10,
                   SPBFILE.SPB_INSTERM_7_11,
                   SPBFILE.SPB_INSRATE_7_11,
                   SPBFILE.SPB_INSTERM_7_12,
                   SPBFILE.SPB_INSRATE_7_12,
                   SPBFILE.SPB_INSTERM_7_13,
                   SPBFILE.SPB_INSRATE_7_13,
                   SPBFILE.SPB_INSTERM_7_14,
                   SPBFILE.SPB_INSRATE_7_14,
                   SPBFILE.SPB_INSTERM_7_15,
                   SPBFILE.SPB_INSRATE_7_15,
                   SPBFILE.SPB_INSTERM_7_16,
                   SPBFILE.SPB_INSRATE_7_16,
                   SPBFILE.SPB_INSTERM_7_17,
                   SPBFILE.SPB_INSRATE_7_17,
                   SPBFILE.SPB_INSTERM_7_18,
                   SPBFILE.SPB_INSRATE_7_18,
                   SPBFILE.SPB_INSTERM_7_19,
                   SPBFILE.SPB_INSRATE_7_19,
                   SPBFILE.SPB_INSTERM_7_20,
                   SPBFILE.SPB_INSRATE_7_20,
                   SPBFILE.SPB_INSMINPREM_7,
                   SPBFILE.SPB_INSMINCHGABL_7,
                   SPBFILE.SPB_INSMAXCHGABL_7,
                   SPBFILE.SPB_INSMAXBENIF_7,
                   SPBFILE.SPB_INSMAXTERM_7,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_7,''),
                   SPBFILE.SPB_INSEXCEPT_7,
                   SPBFILE.SPB_INSNOVER_7,
                   SPBFILE.SPB_INSMINAGE_7_1,
                   SPBFILE.SPB_INSMAXAGE_7_1,
                   SPBFILE.SPB_INSMAXCOV_7_1,
                   SPBFILE.SPB_INSMAXTRM_7_1,
                   SPBFILE.SPB_INSMINAGE_7_2,
                   SPBFILE.SPB_INSMAXAGE_7_2,
                   SPBFILE.SPB_INSMAXCOV_7_2,
                   SPBFILE.SPB_INSMAXTRM_7_2,
                   SPBFILE.SPB_INSMINAGE_7_3,
                   SPBFILE.SPB_INSMAXAGE_7_3,
                   SPBFILE.SPB_INSMAXCOV_7_3,
                   SPBFILE.SPB_INSMAXTRM_7_3,
                   SPBFILE.SPB_INSMINAGE_7_4,
                   SPBFILE.SPB_INSMAXAGE_7_4,
                   SPBFILE.SPB_INSMAXCOV_7_4,
                   SPBFILE.SPB_INSMAXTRM_7_4,
                   SPBFILE.SPB_INSMINAGE_7_5,
                   SPBFILE.SPB_INSMAXAGE_7_5,
                   SPBFILE.SPB_INSMAXCOV_7_5,
                   SPBFILE.SPB_INSMAXTRM_7_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_7,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_7,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_7,''),
                   SPBFILE.SPB_INSCOM_8,
                   SPBFILE.SPB_INSRATECD_8,
                   SPBFILE.SPB_INSJFAC_8,
                   SPBFILE.SPB_INSDISCNT_8,
                   ISNULL(SPBFILE.SPB_INSCHGABL_8,''),
                   SPBFILE.SPB_INSTERM_8_1,
                   SPBFILE.SPB_INSRATE_8_1,
                   SPBFILE.SPB_INSTERM_8_2,
                   SPBFILE.SPB_INSRATE_8_2,
                   SPBFILE.SPB_INSTERM_8_3,
                   SPBFILE.SPB_INSRATE_8_3,
                   SPBFILE.SPB_INSTERM_8_4,
                   SPBFILE.SPB_INSRATE_8_4,
                   SPBFILE.SPB_INSTERM_8_5,
                   SPBFILE.SPB_INSRATE_8_5,
                   SPBFILE.SPB_INSTERM_8_6,
                   SPBFILE.SPB_INSRATE_8_6,
                   SPBFILE.SPB_INSTERM_8_7,
                   SPBFILE.SPB_INSRATE_8_7,
                   SPBFILE.SPB_INSTERM_8_8,
                   SPBFILE.SPB_INSRATE_8_8,
                   SPBFILE.SPB_INSTERM_8_9,
                   SPBFILE.SPB_INSRATE_8_9,
                   SPBFILE.SPB_INSTERM_8_10,
                   SPBFILE.SPB_INSRATE_8_10,
                   SPBFILE.SPB_INSTERM_8_11,
                   SPBFILE.SPB_INSRATE_8_11,
                   SPBFILE.SPB_INSTERM_8_12,
                   SPBFILE.SPB_INSRATE_8_12,
                   SPBFILE.SPB_INSTERM_8_13,
                   SPBFILE.SPB_INSRATE_8_13,
                   SPBFILE.SPB_INSTERM_8_14,
                   SPBFILE.SPB_INSRATE_8_14,
                   SPBFILE.SPB_INSTERM_8_15,
                   SPBFILE.SPB_INSRATE_8_15,
                   SPBFILE.SPB_INSTERM_8_16,
                   SPBFILE.SPB_INSRATE_8_16,
                   SPBFILE.SPB_INSTERM_8_17,
                   SPBFILE.SPB_INSRATE_8_17,
                   SPBFILE.SPB_INSTERM_8_18,
                   SPBFILE.SPB_INSRATE_8_18,
                   SPBFILE.SPB_INSTERM_8_19,
                   SPBFILE.SPB_INSRATE_8_19,
                   SPBFILE.SPB_INSTERM_8_20,
                   SPBFILE.SPB_INSRATE_8_20,
                   SPBFILE.SPB_INSMINPREM_8,
                   SPBFILE.SPB_INSMINCHGABL_8,
                   SPBFILE.SPB_INSMAXCHGABL_8,
                   SPBFILE.SPB_INSMAXBENIF_8,
                   SPBFILE.SPB_INSMAXTERM_8,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_8,''),
                   SPBFILE.SPB_INSEXCEPT_8,
                   SPBFILE.SPB_INSNOVER_8,
                   SPBFILE.SPB_INSMINAGE_8_1,
                   SPBFILE.SPB_INSMAXAGE_8_1,
                   SPBFILE.SPB_INSMAXCOV_8_1,
                   SPBFILE.SPB_INSMAXTRM_8_1,
                   SPBFILE.SPB_INSMINAGE_8_2,
                   SPBFILE.SPB_INSMAXAGE_8_2,
                   SPBFILE.SPB_INSMAXCOV_8_2,
                   SPBFILE.SPB_INSMAXTRM_8_2,
                   SPBFILE.SPB_INSMINAGE_8_3,
                   SPBFILE.SPB_INSMAXAGE_8_3,
                   SPBFILE.SPB_INSMAXCOV_8_3,
                   SPBFILE.SPB_INSMAXTRM_8_3,
                   SPBFILE.SPB_INSMINAGE_8_4,
                   SPBFILE.SPB_INSMAXAGE_8_4,
                   SPBFILE.SPB_INSMAXCOV_8_4,
                   SPBFILE.SPB_INSMAXTRM_8_4,
                   SPBFILE.SPB_INSMINAGE_8_5,
                   SPBFILE.SPB_INSMAXAGE_8_5,
                   SPBFILE.SPB_INSMAXCOV_8_5,
                   SPBFILE.SPB_INSMAXTRM_8_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_8,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_8,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_8,''),
                   SPBFILE.SPB_INSCOM_9,
                   SPBFILE.SPB_INSRATECD_9,
                   SPBFILE.SPB_INSJFAC_9,
                   SPBFILE.SPB_INSDISCNT_9,
                   ISNULL(SPBFILE.SPB_INSCHGABL_9,''),
                   SPBFILE.SPB_INSTERM_9_1,
                   SPBFILE.SPB_INSRATE_9_1,
                   SPBFILE.SPB_INSTERM_9_2,
                   SPBFILE.SPB_INSRATE_9_2,
                   SPBFILE.SPB_INSTERM_9_3,
                   SPBFILE.SPB_INSRATE_9_3,
                   SPBFILE.SPB_INSTERM_9_4,
                   SPBFILE.SPB_INSRATE_9_4,
                   SPBFILE.SPB_INSTERM_9_5,
                   SPBFILE.SPB_INSRATE_9_5,
                   SPBFILE.SPB_INSTERM_9_6,
                   SPBFILE.SPB_INSRATE_9_6,
                   SPBFILE.SPB_INSTERM_9_7,
                   SPBFILE.SPB_INSRATE_9_7,
                   SPBFILE.SPB_INSTERM_9_8,
                   SPBFILE.SPB_INSRATE_9_8,
                   SPBFILE.SPB_INSTERM_9_9,
                   SPBFILE.SPB_INSRATE_9_9,
                   SPBFILE.SPB_INSTERM_9_10,
                   SPBFILE.SPB_INSRATE_9_10,
                   SPBFILE.SPB_INSTERM_9_11,
                   SPBFILE.SPB_INSRATE_9_11,
                   SPBFILE.SPB_INSTERM_9_12,
                   SPBFILE.SPB_INSRATE_9_12,
                   SPBFILE.SPB_INSTERM_9_13,
                   SPBFILE.SPB_INSRATE_9_13,
                   SPBFILE.SPB_INSTERM_9_14,
                   SPBFILE.SPB_INSRATE_9_14,
                   SPBFILE.SPB_INSTERM_9_15,
                   SPBFILE.SPB_INSRATE_9_15,
                   SPBFILE.SPB_INSTERM_9_16,
                   SPBFILE.SPB_INSRATE_9_16,
                   SPBFILE.SPB_INSTERM_9_17,
                   SPBFILE.SPB_INSRATE_9_17,
                   SPBFILE.SPB_INSTERM_9_18,
                   SPBFILE.SPB_INSRATE_9_18,
                   SPBFILE.SPB_INSTERM_9_19,
                   SPBFILE.SPB_INSRATE_9_19,
                   SPBFILE.SPB_INSTERM_9_20,
                   SPBFILE.SPB_INSRATE_9_20,
                   SPBFILE.SPB_INSMINPREM_9,
                   SPBFILE.SPB_INSMINCHGABL_9,
                   SPBFILE.SPB_INSMAXCHGABL_9,
                   SPBFILE.SPB_INSMAXBENIF_9,
                   SPBFILE.SPB_INSMAXTERM_9,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_9,''),
                   SPBFILE.SPB_INSEXCEPT_9,
                   SPBFILE.SPB_INSNOVER_9,
                   SPBFILE.SPB_INSMINAGE_9_1,
                   SPBFILE.SPB_INSMAXAGE_9_1,
                   SPBFILE.SPB_INSMAXCOV_9_1,
                   SPBFILE.SPB_INSMAXTRM_9_1,
                   SPBFILE.SPB_INSMINAGE_9_2,
                   SPBFILE.SPB_INSMAXAGE_9_2,
                   SPBFILE.SPB_INSMAXCOV_9_2,
                   SPBFILE.SPB_INSMAXTRM_9_2,
                   SPBFILE.SPB_INSMINAGE_9_3,
                   SPBFILE.SPB_INSMAXAGE_9_3,
                   SPBFILE.SPB_INSMAXCOV_9_3,
                   SPBFILE.SPB_INSMAXTRM_9_3,
                   SPBFILE.SPB_INSMINAGE_9_4,
                   SPBFILE.SPB_INSMAXAGE_9_4,
                   SPBFILE.SPB_INSMAXCOV_9_4,
                   SPBFILE.SPB_INSMAXTRM_9_4,
                   SPBFILE.SPB_INSMINAGE_9_5,
                   SPBFILE.SPB_INSMAXAGE_9_5,
                   SPBFILE.SPB_INSMAXCOV_9_5,
                   SPBFILE.SPB_INSMAXTRM_9_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_9,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_9,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_9,''),
                   SPBFILE.SPB_INSCOM_10,
                   SPBFILE.SPB_INSRATECD_10,
                   SPBFILE.SPB_INSJFAC_10,
                   SPBFILE.SPB_INSDISCNT_10,
                   ISNULL(SPBFILE.SPB_INSCHGABL_10,''),
                   SPBFILE.SPB_INSTERM_10_1,
                   SPBFILE.SPB_INSRATE_10_1,
                   SPBFILE.SPB_INSTERM_10_2,
                   SPBFILE.SPB_INSRATE_10_2,
                   SPBFILE.SPB_INSTERM_10_3,
                   SPBFILE.SPB_INSRATE_10_3,
                   SPBFILE.SPB_INSTERM_10_4,
                   SPBFILE.SPB_INSRATE_10_4,
                   SPBFILE.SPB_INSTERM_10_5,
                   SPBFILE.SPB_INSRATE_10_5,
                   SPBFILE.SPB_INSTERM_10_6,
                   SPBFILE.SPB_INSRATE_10_6,
                   SPBFILE.SPB_INSTERM_10_7,
                   SPBFILE.SPB_INSRATE_10_7,
                   SPBFILE.SPB_INSTERM_10_8,
                   SPBFILE.SPB_INSRATE_10_8,
                   SPBFILE.SPB_INSTERM_10_9,
                   SPBFILE.SPB_INSRATE_10_9,
                   SPBFILE.SPB_INSTERM_10_10,
                   SPBFILE.SPB_INSRATE_10_10,
                   SPBFILE.SPB_INSTERM_10_11,
                   SPBFILE.SPB_INSRATE_10_11,
                   SPBFILE.SPB_INSTERM_10_12,
                   SPBFILE.SPB_INSRATE_10_12,
                   SPBFILE.SPB_INSTERM_10_13,
                   SPBFILE.SPB_INSRATE_10_13,
                   SPBFILE.SPB_INSTERM_10_14,
                   SPBFILE.SPB_INSRATE_10_14,
                   SPBFILE.SPB_INSTERM_10_15,
                   SPBFILE.SPB_INSRATE_10_15,
                   SPBFILE.SPB_INSTERM_10_16,
                   SPBFILE.SPB_INSRATE_10_16,
                   SPBFILE.SPB_INSTERM_10_17,
                   SPBFILE.SPB_INSRATE_10_17,
                   SPBFILE.SPB_INSTERM_10_18,
                   SPBFILE.SPB_INSRATE_10_18,
                   SPBFILE.SPB_INSTERM_10_19,
                   SPBFILE.SPB_INSRATE_10_19,
                   SPBFILE.SPB_INSTERM_10_20,
                   SPBFILE.SPB_INSRATE_10_20,
                   SPBFILE.SPB_INSMINPREM_10,
                   SPBFILE.SPB_INSMINCHGABL_10,
                   SPBFILE.SPB_INSMAXCHGABL_10,
                   SPBFILE.SPB_INSMAXBENIF_10,
                   SPBFILE.SPB_INSMAXTERM_10,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_10,''),
                   SPBFILE.SPB_INSEXCEPT_10,
                   SPBFILE.SPB_INSNOVER_10,
                   SPBFILE.SPB_INSMINAGE_10_1,
                   SPBFILE.SPB_INSMAXAGE_10_1,
                   SPBFILE.SPB_INSMAXCOV_10_1,
                   SPBFILE.SPB_INSMAXTRM_10_1,
                   SPBFILE.SPB_INSMINAGE_10_2,
                   SPBFILE.SPB_INSMAXAGE_10_2,
                   SPBFILE.SPB_INSMAXCOV_10_2,
                   SPBFILE.SPB_INSMAXTRM_10_2,
                   SPBFILE.SPB_INSMINAGE_10_3,
                   SPBFILE.SPB_INSMAXAGE_10_3,
                   SPBFILE.SPB_INSMAXCOV_10_3,
                   SPBFILE.SPB_INSMAXTRM_10_3,
                   SPBFILE.SPB_INSMINAGE_10_4,
                   SPBFILE.SPB_INSMAXAGE_10_4,
                   SPBFILE.SPB_INSMAXCOV_10_4,
                   SPBFILE.SPB_INSMAXTRM_10_4,
                   SPBFILE.SPB_INSMINAGE_10_5,
                   SPBFILE.SPB_INSMAXAGE_10_5,
                   SPBFILE.SPB_INSMAXCOV_10_5,
                   SPBFILE.SPB_INSMAXTRM_10_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_10,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_10,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_10,''),
                   SPBFILE.SPB_INSCOM_11,
                   SPBFILE.SPB_INSRATECD_11,
                   SPBFILE.SPB_INSJFAC_11,
                   SPBFILE.SPB_INSDISCNT_11,
                   ISNULL(SPBFILE.SPB_INSCHGABL_11,''),
                   SPBFILE.SPB_INSTERM_11_1,
                   SPBFILE.SPB_INSRATE_11_1,
                   SPBFILE.SPB_INSTERM_11_2,
                   SPBFILE.SPB_INSRATE_11_2,
                   SPBFILE.SPB_INSTERM_11_3,
                   SPBFILE.SPB_INSRATE_11_3,
                   SPBFILE.SPB_INSTERM_11_4,
                   SPBFILE.SPB_INSRATE_11_4,
                   SPBFILE.SPB_INSTERM_11_5,
                   SPBFILE.SPB_INSRATE_11_5,
                   SPBFILE.SPB_INSTERM_11_6,
                   SPBFILE.SPB_INSRATE_11_6,
                   SPBFILE.SPB_INSTERM_11_7,
                   SPBFILE.SPB_INSRATE_11_7,
                   SPBFILE.SPB_INSTERM_11_8,
                   SPBFILE.SPB_INSRATE_11_8,
                   SPBFILE.SPB_INSTERM_11_9,
                   SPBFILE.SPB_INSRATE_11_9,
                   SPBFILE.SPB_INSTERM_11_10,
                   SPBFILE.SPB_INSRATE_11_10,
                   SPBFILE.SPB_INSTERM_11_11,
                   SPBFILE.SPB_INSRATE_11_11,
                   SPBFILE.SPB_INSTERM_11_12,
                   SPBFILE.SPB_INSRATE_11_12,
                   SPBFILE.SPB_INSTERM_11_13,
                   SPBFILE.SPB_INSRATE_11_13,
                   SPBFILE.SPB_INSTERM_11_14,
                   SPBFILE.SPB_INSRATE_11_14,
                   SPBFILE.SPB_INSTERM_11_15,
                   SPBFILE.SPB_INSRATE_11_15,
                   SPBFILE.SPB_INSTERM_11_16,
                   SPBFILE.SPB_INSRATE_11_16,
                   SPBFILE.SPB_INSTERM_11_17,
                   SPBFILE.SPB_INSRATE_11_17,
                   SPBFILE.SPB_INSTERM_11_18,
                   SPBFILE.SPB_INSRATE_11_18,
                   SPBFILE.SPB_INSTERM_11_19,
                   SPBFILE.SPB_INSRATE_11_19,
                   SPBFILE.SPB_INSTERM_11_20,
                   SPBFILE.SPB_INSRATE_11_20,
                   SPBFILE.SPB_INSMINPREM_11,
                   SPBFILE.SPB_INSMINCHGABL_11,
                   SPBFILE.SPB_INSMAXCHGABL_11,
                   SPBFILE.SPB_INSMAXBENIF_11,
                   SPBFILE.SPB_INSMAXTERM_11,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_11,''),
                   SPBFILE.SPB_INSEXCEPT_11,
                   SPBFILE.SPB_INSNOVER_11,
                   SPBFILE.SPB_INSMINAGE_11_1,
                   SPBFILE.SPB_INSMAXAGE_11_1,
                   SPBFILE.SPB_INSMAXCOV_11_1,
                   SPBFILE.SPB_INSMAXTRM_11_1,
                   SPBFILE.SPB_INSMINAGE_11_2,
                   SPBFILE.SPB_INSMAXAGE_11_2,
                   SPBFILE.SPB_INSMAXCOV_11_2,
                   SPBFILE.SPB_INSMAXTRM_11_2,
                   SPBFILE.SPB_INSMINAGE_11_3,
                   SPBFILE.SPB_INSMAXAGE_11_3,
                   SPBFILE.SPB_INSMAXCOV_11_3,
                   SPBFILE.SPB_INSMAXTRM_11_3,
                   SPBFILE.SPB_INSMINAGE_11_4,
                   SPBFILE.SPB_INSMAXAGE_11_4,
                   SPBFILE.SPB_INSMAXCOV_11_4,
                   SPBFILE.SPB_INSMAXTRM_11_4,
                   SPBFILE.SPB_INSMINAGE_11_5,
                   SPBFILE.SPB_INSMAXAGE_11_5,
                   SPBFILE.SPB_INSMAXCOV_11_5,
                   SPBFILE.SPB_INSMAXTRM_11_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_11,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_11,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_11,''),
                   SPBFILE.SPB_INSCOM_12,
                   SPBFILE.SPB_INSRATECD_12,
                   SPBFILE.SPB_INSJFAC_12,
                   SPBFILE.SPB_INSDISCNT_12,
                   ISNULL(SPBFILE.SPB_INSCHGABL_12,''),
                   SPBFILE.SPB_INSTERM_12_1,
                   SPBFILE.SPB_INSRATE_12_1,
                   SPBFILE.SPB_INSTERM_12_2,
                   SPBFILE.SPB_INSRATE_12_2,
                   SPBFILE.SPB_INSTERM_12_3,
                   SPBFILE.SPB_INSRATE_12_3,
                   SPBFILE.SPB_INSTERM_12_4,
                   SPBFILE.SPB_INSRATE_12_4,
                   SPBFILE.SPB_INSTERM_12_5,
                   SPBFILE.SPB_INSRATE_12_5,
                   SPBFILE.SPB_INSTERM_12_6,
                   SPBFILE.SPB_INSRATE_12_6,
                   SPBFILE.SPB_INSTERM_12_7,
                   SPBFILE.SPB_INSRATE_12_7,
                   SPBFILE.SPB_INSTERM_12_8,
                   SPBFILE.SPB_INSRATE_12_8,
                   SPBFILE.SPB_INSTERM_12_9,
                   SPBFILE.SPB_INSRATE_12_9,
                   SPBFILE.SPB_INSTERM_12_10,
                   SPBFILE.SPB_INSRATE_12_10,
                   SPBFILE.SPB_INSTERM_12_11,
                   SPBFILE.SPB_INSRATE_12_11,
                   SPBFILE.SPB_INSTERM_12_12,
                   SPBFILE.SPB_INSRATE_12_12,
                   SPBFILE.SPB_INSTERM_12_13,
                   SPBFILE.SPB_INSRATE_12_13,
                   SPBFILE.SPB_INSTERM_12_14,
                   SPBFILE.SPB_INSRATE_12_14,
                   SPBFILE.SPB_INSTERM_12_15,
                   SPBFILE.SPB_INSRATE_12_15,
                   SPBFILE.SPB_INSTERM_12_16,
                   SPBFILE.SPB_INSRATE_12_16,
                   SPBFILE.SPB_INSTERM_12_17,
                   SPBFILE.SPB_INSRATE_12_17,
                   SPBFILE.SPB_INSTERM_12_18,
                   SPBFILE.SPB_INSRATE_12_18,
                   SPBFILE.SPB_INSTERM_12_19,
                   SPBFILE.SPB_INSRATE_12_19,
                   SPBFILE.SPB_INSTERM_12_20,
                   SPBFILE.SPB_INSRATE_12_20,
                   SPBFILE.SPB_INSMINPREM_12,
                   SPBFILE.SPB_INSMINCHGABL_12,
                   SPBFILE.SPB_INSMAXCHGABL_12,
                   SPBFILE.SPB_INSMAXBENIF_12,
                   SPBFILE.SPB_INSMAXTERM_12,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_12,''),
                   SPBFILE.SPB_INSEXCEPT_12,
                   SPBFILE.SPB_INSNOVER_12,
                   SPBFILE.SPB_INSMINAGE_12_1,
                   SPBFILE.SPB_INSMAXAGE_12_1,
                   SPBFILE.SPB_INSMAXCOV_12_1,
                   SPBFILE.SPB_INSMAXTRM_12_1,
                   SPBFILE.SPB_INSMINAGE_12_2,
                   SPBFILE.SPB_INSMAXAGE_12_2,
                   SPBFILE.SPB_INSMAXCOV_12_2,
                   SPBFILE.SPB_INSMAXTRM_12_2,
                   SPBFILE.SPB_INSMINAGE_12_3,
                   SPBFILE.SPB_INSMAXAGE_12_3,
                   SPBFILE.SPB_INSMAXCOV_12_3,
                   SPBFILE.SPB_INSMAXTRM_12_3,
                   SPBFILE.SPB_INSMINAGE_12_4,
                   SPBFILE.SPB_INSMAXAGE_12_4,
                   SPBFILE.SPB_INSMAXCOV_12_4,
                   SPBFILE.SPB_INSMAXTRM_12_4,
                   SPBFILE.SPB_INSMINAGE_12_5,
                   SPBFILE.SPB_INSMAXAGE_12_5,
                   SPBFILE.SPB_INSMAXCOV_12_5,
                   SPBFILE.SPB_INSMAXTRM_12_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_12,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_12,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_12,'')
            INTO :QSPB-REC
            FROM DBO.SPBFILE
            WHERE SPBFILE.SPB_ORGST = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE 
           END-EXEC.

           perform sql-io-validation.

      *-----------------------------------------------------------------
       read-spc1-file.
           move "read" to e-msg4.
           move 0 to io-fg.

           EXEC SQL
            SELECT ISNULL(SPCFILE.SPC_ORGST,''),
                   SPCFILE.SPC_SPRCLASS,
                   SPCFILE.SPC_SUBCLASS,
                   SPCFILE.SPC_LAWCODE,
                   SPCFILE.SPC_INSEARN_ORDER_1,
                   SPCFILE.SPC_INSEARN_ORDER_2,
                   SPCFILE.SPC_INSEARN_ORDER_3,
                   SPCFILE.SPC_INSEARN_ORDER_4,
                   SPCFILE.SPC_INSEARN_ORDER_5,
                   SPCFILE.SPC_INSEARN_ORDER_6,
                   SPCFILE.SPC_INSEARN_ORDER_7,
                   SPCFILE.SPC_INSEARN_ORDER_8,
                   SPCFILE.SPC_INSEARN_ORDER_9,
                   SPCFILE.SPC_INSEARN_ORDER_10,
                   SPCFILE.SPC_INSEARN_ORDER_11,
                   SPCFILE.SPC_INSEARN_ORDER_12,
                   ISNULL(SPCFILE.SPC_INSAH_JOINT_FG,''),
                   ISNULL(SPCFILE.SPC_INS_O1_EDIT,''),
                   ISNULL(SPCFILE.SPC_INS_O2_EDIT,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_RBDTE_1_1,''),
                   SPCFILE.SPC_RBDAYS_1_1,
                   SPCFILE.SPC_RBYRTYPE_1_1,
                   ISNULL(SPCFILE.SPC_RBDTE_1_2,''),
                   SPCFILE.SPC_RBDAYS_1_2,
                   SPCFILE.SPC_RBYRTYPE_1_2,
                   SPCFILE.SPC_RBMINRET_1,
                   SPCFILE.SPC_RBNOMON_1,
                   SPCFILE.SPC_RBDEDCHG_1,
                   SPCFILE.SPC_RBRECAST_1,
                   SPCFILE.SPC_RBEARLY_1,
                   SPCFILE.SPC_RBDEFER_1,
                   SPCFILE.SPC_RBPCT_1,
                   SPCFILE.SPC_RBREDUC_1,
                   SPCFILE.SPC_RBMIN_1,
                   SPCFILE.SPC_RBSPOPT1_1,
                   SPCFILE.SPC_RBSPOPT2_1,
                   SPCFILE.SPC_RBMANAGER_1,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_RBDTE_2_1,''),
                   SPCFILE.SPC_RBDAYS_2_1,
                   SPCFILE.SPC_RBYRTYPE_2_1,
                   ISNULL(SPCFILE.SPC_RBDTE_2_2,''),
                   SPCFILE.SPC_RBDAYS_2_2,
                   SPCFILE.SPC_RBYRTYPE_2_2,
                   SPCFILE.SPC_RBMINRET_2,
                   SPCFILE.SPC_RBNOMON_2,
                   SPCFILE.SPC_RBDEDCHG_2,
                   SPCFILE.SPC_RBRECAST_2,
                   SPCFILE.SPC_RBEARLY_2,
                   SPCFILE.SPC_RBDEFER_2,
                   SPCFILE.SPC_RBPCT_2,
                   SPCFILE.SPC_RBREDUC_2,
                   SPCFILE.SPC_RBMIN_2,
                   SPCFILE.SPC_RBSPOPT1_2,
                   SPCFILE.SPC_RBSPOPT2_2,
                   SPCFILE.SPC_RBMANAGER_2,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_3,''),
                   ISNULL(SPCFILE.SPC_RBDTE_3_1,''),
                   SPCFILE.SPC_RBDAYS_3_1,
                   SPCFILE.SPC_RBYRTYPE_3_1,
                   ISNULL(SPCFILE.SPC_RBDTE_3_2,''),
                   SPCFILE.SPC_RBDAYS_3_2,
                   SPCFILE.SPC_RBYRTYPE_3_2,
                   SPCFILE.SPC_RBMINRET_3,
                   SPCFILE.SPC_RBNOMON_3,
                   SPCFILE.SPC_RBDEDCHG_3,
                   SPCFILE.SPC_RBRECAST_3,
                   SPCFILE.SPC_RBEARLY_3,
                   SPCFILE.SPC_RBDEFER_3,
                   SPCFILE.SPC_RBPCT_3,
                   SPCFILE.SPC_RBREDUC_3,
                   SPCFILE.SPC_RBMIN_3,
                   SPCFILE.SPC_RBSPOPT1_3,
                   SPCFILE.SPC_RBSPOPT2_3,
                   SPCFILE.SPC_RBMANAGER_3,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_4,''),
                   ISNULL(SPCFILE.SPC_RBDTE_4_1,''),
                   SPCFILE.SPC_RBDAYS_4_1,
                   SPCFILE.SPC_RBYRTYPE_4_1,
                   ISNULL(SPCFILE.SPC_RBDTE_4_2,''),
                   SPCFILE.SPC_RBDAYS_4_2,
                   SPCFILE.SPC_RBYRTYPE_4_2,
                   SPCFILE.SPC_RBMINRET_4,
                   SPCFILE.SPC_RBNOMON_4,
                   SPCFILE.SPC_RBDEDCHG_4,
                   SPCFILE.SPC_RBRECAST_4,
                   SPCFILE.SPC_RBEARLY_4,
                   SPCFILE.SPC_RBDEFER_4,
                   SPCFILE.SPC_RBPCT_4,
                   SPCFILE.SPC_RBREDUC_4,
                   SPCFILE.SPC_RBMIN_4,
                   SPCFILE.SPC_RBSPOPT1_4,
                   SPCFILE.SPC_RBSPOPT2_4,
                   SPCFILE.SPC_RBMANAGER_4,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_5,''),
                   ISNULL(SPCFILE.SPC_RBDTE_5_1,''),
                   SPCFILE.SPC_RBDAYS_5_1,
                   SPCFILE.SPC_RBYRTYPE_5_1,
                   ISNULL(SPCFILE.SPC_RBDTE_5_2,''),
                   SPCFILE.SPC_RBDAYS_5_2,
                   SPCFILE.SPC_RBYRTYPE_5_2,
                   SPCFILE.SPC_RBMINRET_5,
                   SPCFILE.SPC_RBNOMON_5,
                   SPCFILE.SPC_RBDEDCHG_5,
                   SPCFILE.SPC_RBRECAST_5,
                   SPCFILE.SPC_RBEARLY_5,
                   SPCFILE.SPC_RBDEFER_5,
                   SPCFILE.SPC_RBPCT_5,
                   SPCFILE.SPC_RBREDUC_5,
                   SPCFILE.SPC_RBMIN_5,
                   SPCFILE.SPC_RBSPOPT1_5,
                   SPCFILE.SPC_RBSPOPT2_5,
                   SPCFILE.SPC_RBMANAGER_5,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_6,''),
                   ISNULL(SPCFILE.SPC_RBDTE_6_1,''),
                   SPCFILE.SPC_RBDAYS_6_1,
                   SPCFILE.SPC_RBYRTYPE_6_1,
                   ISNULL(SPCFILE.SPC_RBDTE_6_2,''),
                   SPCFILE.SPC_RBDAYS_6_2,
                   SPCFILE.SPC_RBYRTYPE_6_2,
                   SPCFILE.SPC_RBMINRET_6,
                   SPCFILE.SPC_RBNOMON_6,
                   SPCFILE.SPC_RBDEDCHG_6,
                   SPCFILE.SPC_RBRECAST_6,
                   SPCFILE.SPC_RBEARLY_6,
                   SPCFILE.SPC_RBDEFER_6,
                   SPCFILE.SPC_RBPCT_6,
                   SPCFILE.SPC_RBREDUC_6,
                   SPCFILE.SPC_RBMIN_6,
                   SPCFILE.SPC_RBSPOPT1_6,
                   SPCFILE.SPC_RBSPOPT2_6,
                   SPCFILE.SPC_RBMANAGER_6,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_7,''),
                   ISNULL(SPCFILE.SPC_RBDTE_7_1,''),
                   SPCFILE.SPC_RBDAYS_7_1,
                   SPCFILE.SPC_RBYRTYPE_7_1,
                   ISNULL(SPCFILE.SPC_RBDTE_7_2,''),
                   SPCFILE.SPC_RBDAYS_7_2,
                   SPCFILE.SPC_RBYRTYPE_7_2,
                   SPCFILE.SPC_RBMINRET_7,
                   SPCFILE.SPC_RBNOMON_7,
                   SPCFILE.SPC_RBDEDCHG_7,
                   SPCFILE.SPC_RBRECAST_7,
                   SPCFILE.SPC_RBEARLY_7,
                   SPCFILE.SPC_RBDEFER_7,
                   SPCFILE.SPC_RBPCT_7,
                   SPCFILE.SPC_RBREDUC_7,
                   SPCFILE.SPC_RBMIN_7,
                   SPCFILE.SPC_RBSPOPT1_7,
                   SPCFILE.SPC_RBSPOPT2_7,
                   SPCFILE.SPC_RBMANAGER_7,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_8,''),
                   ISNULL(SPCFILE.SPC_RBDTE_8_1,''),
                   SPCFILE.SPC_RBDAYS_8_1,
                   SPCFILE.SPC_RBYRTYPE_8_1,
                   ISNULL(SPCFILE.SPC_RBDTE_8_2,''),
                   SPCFILE.SPC_RBDAYS_8_2,
                   SPCFILE.SPC_RBYRTYPE_8_2,
                   SPCFILE.SPC_RBMINRET_8,
                   SPCFILE.SPC_RBNOMON_8,
                   SPCFILE.SPC_RBDEDCHG_8,
                   SPCFILE.SPC_RBRECAST_8,
                   SPCFILE.SPC_RBEARLY_8,
                   SPCFILE.SPC_RBDEFER_8,
                   SPCFILE.SPC_RBPCT_8,
                   SPCFILE.SPC_RBREDUC_8,
                   SPCFILE.SPC_RBMIN_8,
                   SPCFILE.SPC_RBSPOPT1_8,
                   SPCFILE.SPC_RBSPOPT2_8,
                   SPCFILE.SPC_RBMANAGER_8,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_9,''),
                   ISNULL(SPCFILE.SPC_RBDTE_9_1,''),
                   SPCFILE.SPC_RBDAYS_9_1,
                   SPCFILE.SPC_RBYRTYPE_9_1,
                   ISNULL(SPCFILE.SPC_RBDTE_9_2,''),
                   SPCFILE.SPC_RBDAYS_9_2,
                   SPCFILE.SPC_RBYRTYPE_9_2,
                   SPCFILE.SPC_RBMINRET_9,
                   SPCFILE.SPC_RBNOMON_9,
                   SPCFILE.SPC_RBDEDCHG_9,
                   SPCFILE.SPC_RBRECAST_9,
                   SPCFILE.SPC_RBEARLY_9,
                   SPCFILE.SPC_RBDEFER_9,
                   SPCFILE.SPC_RBPCT_9,
                   SPCFILE.SPC_RBREDUC_9,
                   SPCFILE.SPC_RBMIN_9,
                   SPCFILE.SPC_RBSPOPT1_9,
                   SPCFILE.SPC_RBSPOPT2_9,
                   SPCFILE.SPC_RBMANAGER_9,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_10,''),
                   ISNULL(SPCFILE.SPC_RBDTE_10_1,''),
                   SPCFILE.SPC_RBDAYS_10_1,
                   SPCFILE.SPC_RBYRTYPE_10_1,
                   ISNULL(SPCFILE.SPC_RBDTE_10_2,''),
                   SPCFILE.SPC_RBDAYS_10_2,
                   SPCFILE.SPC_RBYRTYPE_10_2,
                   SPCFILE.SPC_RBMINRET_10,
                   SPCFILE.SPC_RBNOMON_10,
                   SPCFILE.SPC_RBDEDCHG_10,
                   SPCFILE.SPC_RBRECAST_10,
                   SPCFILE.SPC_RBEARLY_10,
                   SPCFILE.SPC_RBDEFER_10,
                   SPCFILE.SPC_RBPCT_10,
                   SPCFILE.SPC_RBREDUC_10,
                   SPCFILE.SPC_RBMIN_10,
                   SPCFILE.SPC_RBSPOPT1_10,
                   SPCFILE.SPC_RBSPOPT2_10,
                   SPCFILE.SPC_RBMANAGER_10,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_11,''),
                   ISNULL(SPCFILE.SPC_RBDTE_11_1,''),
                   SPCFILE.SPC_RBDAYS_11_1,
                   SPCFILE.SPC_RBYRTYPE_11_1,
                   ISNULL(SPCFILE.SPC_RBDTE_11_2,''),
                   SPCFILE.SPC_RBDAYS_11_2,
                   SPCFILE.SPC_RBYRTYPE_11_2,
                   SPCFILE.SPC_RBMINRET_11,
                   SPCFILE.SPC_RBNOMON_11,
                   SPCFILE.SPC_RBDEDCHG_11,
                   SPCFILE.SPC_RBRECAST_11,
                   SPCFILE.SPC_RBEARLY_11,
                   SPCFILE.SPC_RBDEFER_11,
                   SPCFILE.SPC_RBPCT_11,
                   SPCFILE.SPC_RBREDUC_11,
                   SPCFILE.SPC_RBMIN_11,
                   SPCFILE.SPC_RBSPOPT1_11,
                   SPCFILE.SPC_RBSPOPT2_11,
                   SPCFILE.SPC_RBMANAGER_11,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_12,''),
                   ISNULL(SPCFILE.SPC_RBDTE_12_1,''),
                   SPCFILE.SPC_RBDAYS_12_1,
                   SPCFILE.SPC_RBYRTYPE_12_1,
                   ISNULL(SPCFILE.SPC_RBDTE_12_2,''),
                   SPCFILE.SPC_RBDAYS_12_2,
                   SPCFILE.SPC_RBYRTYPE_12_2,
                   SPCFILE.SPC_RBMINRET_12,
                   SPCFILE.SPC_RBNOMON_12,
                   SPCFILE.SPC_RBDEDCHG_12,
                   SPCFILE.SPC_RBRECAST_12,
                   SPCFILE.SPC_RBEARLY_12,
                   SPCFILE.SPC_RBDEFER_12,
                   SPCFILE.SPC_RBPCT_12,
                   SPCFILE.SPC_RBREDUC_12,
                   SPCFILE.SPC_RBMIN_12,
                   SPCFILE.SPC_RBSPOPT1_12,
                   SPCFILE.SPC_RBSPOPT2_12,
                   SPCFILE.SPC_RBMANAGER_12,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_13,''),
                   ISNULL(SPCFILE.SPC_RBDTE_13_1,''),
                   SPCFILE.SPC_RBDAYS_13_1,
                   SPCFILE.SPC_RBYRTYPE_13_1,
                   ISNULL(SPCFILE.SPC_RBDTE_13_2,''),
                   SPCFILE.SPC_RBDAYS_13_2,
                   SPCFILE.SPC_RBYRTYPE_13_2,
                   SPCFILE.SPC_RBMINRET_13,
                   SPCFILE.SPC_RBNOMON_13,
                   SPCFILE.SPC_RBDEDCHG_13,
                   SPCFILE.SPC_RBRECAST_13,
                   SPCFILE.SPC_RBEARLY_13,
                   SPCFILE.SPC_RBDEFER_13,
                   SPCFILE.SPC_RBPCT_13,
                   SPCFILE.SPC_RBREDUC_13,
                   SPCFILE.SPC_RBMIN_13,
                   SPCFILE.SPC_RBSPOPT1_13,
                   SPCFILE.SPC_RBSPOPT2_13,
                   SPCFILE.SPC_RBMANAGER_13,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_14,''),
                   ISNULL(SPCFILE.SPC_RBDTE_14_1,''),
                   SPCFILE.SPC_RBDAYS_14_1,
                   SPCFILE.SPC_RBYRTYPE_14_1,
                   ISNULL(SPCFILE.SPC_RBDTE_14_2,''),
                   SPCFILE.SPC_RBDAYS_14_2,
                   SPCFILE.SPC_RBYRTYPE_14_2,
                   SPCFILE.SPC_RBMINRET_14,
                   SPCFILE.SPC_RBNOMON_14,
                   SPCFILE.SPC_RBDEDCHG_14,
                   SPCFILE.SPC_RBRECAST_14,
                   SPCFILE.SPC_RBEARLY_14,
                   SPCFILE.SPC_RBDEFER_14,
                   SPCFILE.SPC_RBPCT_14,
                   SPCFILE.SPC_RBREDUC_14,
                   SPCFILE.SPC_RBMIN_14,
                   SPCFILE.SPC_RBSPOPT1_14,
                   SPCFILE.SPC_RBSPOPT2_14,
                   SPCFILE.SPC_RBMANAGER_14,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_15,''),
                   ISNULL(SPCFILE.SPC_RBDTE_15_1,''),
                   SPCFILE.SPC_RBDAYS_15_1,
                   SPCFILE.SPC_RBYRTYPE_15_1,
                   ISNULL(SPCFILE.SPC_RBDTE_15_2,''),
                   SPCFILE.SPC_RBDAYS_15_2,
                   SPCFILE.SPC_RBYRTYPE_15_2,
                   SPCFILE.SPC_RBMINRET_15,
                   SPCFILE.SPC_RBNOMON_15,
                   SPCFILE.SPC_RBDEDCHG_15,
                   SPCFILE.SPC_RBRECAST_15,
                   SPCFILE.SPC_RBEARLY_15,
                   SPCFILE.SPC_RBDEFER_15,
                   SPCFILE.SPC_RBPCT_15,
                   SPCFILE.SPC_RBREDUC_15,
                   SPCFILE.SPC_RBMIN_15,
                   SPCFILE.SPC_RBSPOPT1_15,
                   SPCFILE.SPC_RBSPOPT2_15,
                   SPCFILE.SPC_RBMANAGER_15,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_16,''),
                   ISNULL(SPCFILE.SPC_RBDTE_16_1,''),
                   SPCFILE.SPC_RBDAYS_16_1,
                   SPCFILE.SPC_RBYRTYPE_16_1,
                   ISNULL(SPCFILE.SPC_RBDTE_16_2,''),
                   SPCFILE.SPC_RBDAYS_16_2,
                   SPCFILE.SPC_RBYRTYPE_16_2,
                   SPCFILE.SPC_RBMINRET_16,
                   SPCFILE.SPC_RBNOMON_16,
                   SPCFILE.SPC_RBDEDCHG_16,
                   SPCFILE.SPC_RBRECAST_16,
                   SPCFILE.SPC_RBEARLY_16,
                   SPCFILE.SPC_RBDEFER_16,
                   SPCFILE.SPC_RBPCT_16,
                   SPCFILE.SPC_RBREDUC_16,
                   SPCFILE.SPC_RBMIN_16,
                   SPCFILE.SPC_RBSPOPT1_16,
                   SPCFILE.SPC_RBSPOPT2_16,
                   SPCFILE.SPC_RBMANAGER_16,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_17,''),
                   ISNULL(SPCFILE.SPC_RBDTE_17_1,''),
                   SPCFILE.SPC_RBDAYS_17_1,
                   SPCFILE.SPC_RBYRTYPE_17_1,
                   ISNULL(SPCFILE.SPC_RBDTE_17_2,''),
                   SPCFILE.SPC_RBDAYS_17_2,
                   SPCFILE.SPC_RBYRTYPE_17_2,
                   SPCFILE.SPC_RBMINRET_17,
                   SPCFILE.SPC_RBNOMON_17,
                   SPCFILE.SPC_RBDEDCHG_17,
                   SPCFILE.SPC_RBRECAST_17,
                   SPCFILE.SPC_RBEARLY_17,
                   SPCFILE.SPC_RBDEFER_17,
                   SPCFILE.SPC_RBPCT_17,
                   SPCFILE.SPC_RBREDUC_17,
                   SPCFILE.SPC_RBMIN_17,
                   SPCFILE.SPC_RBSPOPT1_17,
                   SPCFILE.SPC_RBSPOPT2_17,
                   SPCFILE.SPC_RBMANAGER_17,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_18,''),
                   ISNULL(SPCFILE.SPC_RBDTE_18_1,''),
                   SPCFILE.SPC_RBDAYS_18_1,
                   SPCFILE.SPC_RBYRTYPE_18_1,
                   ISNULL(SPCFILE.SPC_RBDTE_18_2,''),
                   SPCFILE.SPC_RBDAYS_18_2,
                   SPCFILE.SPC_RBYRTYPE_18_2,
                   SPCFILE.SPC_RBMINRET_18,
                   SPCFILE.SPC_RBNOMON_18,
                   SPCFILE.SPC_RBDEDCHG_18,
                   SPCFILE.SPC_RBRECAST_18,
                   SPCFILE.SPC_RBEARLY_18,
                   SPCFILE.SPC_RBDEFER_18,
                   SPCFILE.SPC_RBPCT_18,
                   SPCFILE.SPC_RBREDUC_18,
                   SPCFILE.SPC_RBMIN_18,
                   SPCFILE.SPC_RBSPOPT1_18,
                   SPCFILE.SPC_RBSPOPT2_18,
                   SPCFILE.SPC_RBMANAGER_18,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_19,''),
                   ISNULL(SPCFILE.SPC_RBDTE_19_1,''),
                   SPCFILE.SPC_RBDAYS_19_1,
                   SPCFILE.SPC_RBYRTYPE_19_1,
                   ISNULL(SPCFILE.SPC_RBDTE_19_2,''),
                   SPCFILE.SPC_RBDAYS_19_2,
                   SPCFILE.SPC_RBYRTYPE_19_2,
                   SPCFILE.SPC_RBMINRET_19,
                   SPCFILE.SPC_RBNOMON_19,
                   SPCFILE.SPC_RBDEDCHG_19,
                   SPCFILE.SPC_RBRECAST_19,
                   SPCFILE.SPC_RBEARLY_19,
                   SPCFILE.SPC_RBDEFER_19,
                   SPCFILE.SPC_RBPCT_19,
                   SPCFILE.SPC_RBREDUC_19,
                   SPCFILE.SPC_RBMIN_19,
                   SPCFILE.SPC_RBSPOPT1_19,
                   SPCFILE.SPC_RBSPOPT2_19,
                   SPCFILE.SPC_RBMANAGER_19,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_20,''),
                   ISNULL(SPCFILE.SPC_RBDTE_20_1,''),
                   SPCFILE.SPC_RBDAYS_20_1,
                   SPCFILE.SPC_RBYRTYPE_20_1,
                   ISNULL(SPCFILE.SPC_RBDTE_20_2,''),
                   SPCFILE.SPC_RBDAYS_20_2,
                   SPCFILE.SPC_RBYRTYPE_20_2,
                   SPCFILE.SPC_RBMINRET_20,
                   SPCFILE.SPC_RBNOMON_20,
                   SPCFILE.SPC_RBDEDCHG_20,
                   SPCFILE.SPC_RBRECAST_20,
                   SPCFILE.SPC_RBEARLY_20,
                   SPCFILE.SPC_RBDEFER_20,
                   SPCFILE.SPC_RBPCT_20,
                   SPCFILE.SPC_RBREDUC_20,
                   SPCFILE.SPC_RBMIN_20,
                   SPCFILE.SPC_RBSPOPT1_20,
                   SPCFILE.SPC_RBSPOPT2_20,
                   SPCFILE.SPC_RBMANAGER_20,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_21,''),
                   ISNULL(SPCFILE.SPC_RBDTE_21_1,''),
                   SPCFILE.SPC_RBDAYS_21_1,
                   SPCFILE.SPC_RBYRTYPE_21_1,
                   ISNULL(SPCFILE.SPC_RBDTE_21_2,''),
                   SPCFILE.SPC_RBDAYS_21_2,
                   SPCFILE.SPC_RBYRTYPE_21_2,
                   SPCFILE.SPC_RBMINRET_21,
                   SPCFILE.SPC_RBNOMON_21,
                   SPCFILE.SPC_RBDEDCHG_21,
                   SPCFILE.SPC_RBRECAST_21,
                   SPCFILE.SPC_RBEARLY_21,
                   SPCFILE.SPC_RBDEFER_21,
                   SPCFILE.SPC_RBPCT_21,
                   SPCFILE.SPC_RBREDUC_21,
                   SPCFILE.SPC_RBMIN_21,
                   SPCFILE.SPC_RBSPOPT1_21,
                   SPCFILE.SPC_RBSPOPT2_21,
                   SPCFILE.SPC_RBMANAGER_21,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_22,''),
                   ISNULL(SPCFILE.SPC_RBDTE_22_1,''),
                   SPCFILE.SPC_RBDAYS_22_1,
                   SPCFILE.SPC_RBYRTYPE_22_1,
                   ISNULL(SPCFILE.SPC_RBDTE_22_2,''),
                   SPCFILE.SPC_RBDAYS_22_2,
                   SPCFILE.SPC_RBYRTYPE_22_2,
                   SPCFILE.SPC_RBMINRET_22,
                   SPCFILE.SPC_RBNOMON_22,
                   SPCFILE.SPC_RBDEDCHG_22,
                   SPCFILE.SPC_RBRECAST_22,
                   SPCFILE.SPC_RBEARLY_22,
                   SPCFILE.SPC_RBDEFER_22,
                   SPCFILE.SPC_RBPCT_22,
                   SPCFILE.SPC_RBREDUC_22,
                   SPCFILE.SPC_RBMIN_22,
                   SPCFILE.SPC_RBSPOPT1_22,
                   SPCFILE.SPC_RBSPOPT2_22,
                   SPCFILE.SPC_RBMANAGER_22,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_22,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_22,''),
                   ISNULL(SPCFILE.SPC_RBCOMCD,''),
                   ISNULL(SPCFILE.SPC_ADDON_DISFRMLA,''),
                   ISNULL(SPCFILE.SPC_ADDON_WHOLE_FG,''),
                   ISNULL(SPCFILE.SPC_ADDON_ORG_MATURITY,''),
                   ISNULL(SPCFILE.SPC_CONSOLIDATE_RT_FG,''),
                   SPCFILE.SPC_1STPY_ADDON_DAYS,
                   SPCFILE.SPC_1STPY_ADDON_YRTYPE,
                   ISNULL(SPCFILE.SPC_ADDON_RATE_FG,''),
                   SPCFILE.SPC_ADDON_RATE,
                   ISNULL(SPCFILE.SPC_FEEFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_FEECD_1,''),
                   ISNULL(SPCFILE.SPC_FEEID_1,''),
                   ISNULL(SPCFILE.SPC_FEEFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_FEECD_2,''),
                   ISNULL(SPCFILE.SPC_FEEID_2,''),
                   ISNULL(SPCFILE.SPC_ROUND_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_IRREGULAR_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_1,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_2,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_3,''),
                   ISNULL(SPCFILE.SPC_PARVHELD_RB_CD,''),
                   ISNULL(SPCFILE.SPC_PARVPAID_RB_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   ISNULL(SPCFILE.SPC_OPT_CAL_CHGABLE,''),
                   SPCFILE.SPC_PBDEL_DAYS,
                   SPCFILE.SPC_PBINCR_PERCENT,
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   SPCFILE.SPC_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_RESCIND_BUS_DAYS,''),
                   ISNULL(SPCFILE.SPC_RESCIND_SATURDAY,''),
                   SPCFILE.SPC_MIN_CREDIT_SCORE,
                   ISNULL(SPCFILE.SPC_DUAL_IN_BRANCH,''),
                   ISNULL(SPCFILE.SPC_DUAL_IN_STATE,''),
                   ISNULL(SPCFILE.SPC_PACESETTER_FG,''),
                   SPCFILE.SPC_WAGE_ASSIGN_DAYS,
                   ISNULL(SPCFILE.SPC_CAL_PAYMENT_ROUND_FG,''),
                   ISNULL(SPCFILE.SPC_CAN_LOANS_FG,''),
                   ISNULL(SPCFILE.SPC_ALLOW_DELINQ_POLCD,''),
                   SPCFILE.SPC_YTD_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_LNCOST_FRMLA,''),
                   SPCFILE.SPC_LNCOST_AMT,
                   SPCFILE.SPC_DLDISC_RVPRIN1_PCNT,
                   SPCFILE.SPC_RENEWABLE_PAYS,
                   SPCFILE.SPC_RENEWABLE_DAYS,
                   ISNULL(SPCFILE.SPC_ALLOW_LN_BACK_DATE,''),
                   SPCFILE.SPC_DLDISC_RVPRIN1_AF_PCNT,
                   ISNULL(SPCFILE.SPC_CAL_MAX_AT_ESTIMATE,''),
                   ISNULL(SPCFILE.SPC_INSAH_RETRO,''),
                   ISNULL(SPCFILE.SPC_INSAH_DAYS_WAIT,''),
                   ISNULL(SPCFILE.SPC_ADDON_EXCEPT_FG,''),
                   ISNULL(SPCFILE.SPC_REPO_TRCD_1,''),
                   SPCFILE.SPC_REPO_FMNO_1_1,
                   SPCFILE.SPC_REPO_FMNO_1_2,
                   SPCFILE.SPC_REPO_FMNO_1_3,
                   SPCFILE.SPC_REPO_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_2,''),
                   SPCFILE.SPC_REPO_FMNO_2_1,
                   SPCFILE.SPC_REPO_FMNO_2_2,
                   SPCFILE.SPC_REPO_FMNO_2_3,
                   SPCFILE.SPC_REPO_FMNO_2_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_3,''),
                   SPCFILE.SPC_REPO_FMNO_3_1,
                   SPCFILE.SPC_REPO_FMNO_3_2,
                   SPCFILE.SPC_REPO_FMNO_3_3,
                   SPCFILE.SPC_REPO_FMNO_3_4,
                   ISNULL(SPCFILE.SPC_CANCEL_ADDON_FG,''),
                   SPCFILE.SPC_DEFAULT_O2_INS_COMP,
                   ISNULL(SPCFILE.SPC_LC_ON_JUDGEMENTS,''),
                   SPCFILE.SPC_DELQ_IB_AGE,
                   SPCFILE.SPC_DELQ_IB_MAX_DAYS,
                   ISNULL(SPCFILE.SPC_INSURANCE_WRITTEN,''),
                   ISNULL(SPCFILE.SPC_INTERIM_COUPON,''),
                   ISNULL(SPCFILE.SPC_AUTO_ASSESS_LCHG,''),
                   SPCFILE.SPC_MAX_CASH_PROCEEDS,
                   ISNULL(SPCFILE.SPC_ALLOW_ZERO_PROC,''),
                   SPCFILE.SPC_BANKRULE_RATE,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_1,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_2,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_4,
                   SPCFILE.SPC_DLDISC_CONV_BF_PCNT,
                   ISNULL(SPCFILE.SPC_UNITPER_CD,''),
                   ISNULL(SPCFILE.SPC_RENEWAL_EDIT,''),
                   ISNULL(SPCFILE.SPC_ROUND_MFEE,''),
                   ISNULL(SPCFILE.SPC_ADV_TO_1ST_NO_CORR,''),
                   ISNULL(SPCFILE.SPC_LC_INT_ON_REPO_NON_PL,''),
                   SPCFILE.SPC_CONTRACTUAL_SET_NONACCRUAL,
                   SPCFILE.SPC_CONTRACTUAL_RESET_ACCRUAL,
                   SPCFILE.SPC_MONTHS_PAY_NAPP_COUNTER,
                   SPCFILE.SPC_SHORT_PAY_THRESHOLD,
                   SPCFILE.SPC_CASH_AVAIL_PERCENT,
                   ISNULL(SPCFILE.SPC_O1_JOINT_FG,''),
                   SPCFILE.SPC_NET_BAL_REQ,
                   SPCFILE.SPC_AMT_PROCEEDS_REQ,
                   SPCFILE.SPC_ALT_PREPAY_MONTHS,
                   SPCFILE.SPC_PERCENT_REGPAY_AVAIL,
                   SPCFILE.SPC_ALT_DEFFRMLA,
                   SPCFILE.SPC_ALT_DEFMIN,
                   SPCFILE.SPC_ALT_DEFMAX,
                   ISNULL(SPCFILE.SPC_NON_LOAN_FLAG,''),
                   ISNULL(SPCFILE.SPC_SKIP_TRW,'')
            INTO :QSPC-REC
            FROM DBO.SPCFILE
            WHERE SPCFILE.SPC_ORGST = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE 
           END-EXEC.

           perform sql-io-validation.

      *-----------------------------------------------------------------
       sql-io-validation.

           if ( e-msg4  = "open" ) and   *> good
              ( sqlcode = 1      )
              exit paragraph
           else
           if ( sqlcode = 0      )       *> good
              exit paragraph.

           move 9 to io-fg.

           if ( e-msg4  = "read" ) and   *> eof or row not found
              ( sqlcode = 100    )
              exit paragraph.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *copy "common.cpy".
       initialization-routine.
           display "Enter the server name: ", no advancing.
           accept dsn-name.
           display "Enter your user id (or press RETURN to use Windows A
      -           "uthentication): ", no.
           accept userid.
	   if userid not = spaces
               display "Enter your password : ", no
               accept passwd-name

      * Passwords in a CONNECT statement must be entered in a VARCHAR format
      * with the length of the input string.
      *kc      inspect passwd-name tallying passwd-length for characters
      *kc         before initial " "
	   end-if.

       make-connection.
      *    if userid not = spaces
      *       EXEC SQL CONNECT TO :dsn-name as C1
      *                      USER :userid USING :passwd
      *       END-EXEC
      *    else
      *       EXEC SQL CONNECT TO :dsn-name as C1
      *       END-EXEC
      *    end-if.
      *    if sqlcode < 0 perform error-exit.
      *    if sqlcode = 0 display "Connection successful".

           accept sqlserver from environment
                        "otis_dsn_name".
           accept userid from environment "otis_acusql_login".
           accept passwd-name from environment "otis_acusql_pswd".

           exec sql connect to :sqlserver as C1
                          user :userid using :passwd-name
           end-exec.
      
           exec sql whenever sqlerror go to error-exit end-exec.
      
           if sqlcode < 0 perform error-exit.
      
      
      *kc  exec sql connect to :sqlserver as connect2
      *kc                 user :userid using :passwd
      *kc  end-exec.
      
      *    exec sql connect to :sqlserver as connect2
      *                   user :userid using :passwd-name
      *    end-exec.
      
           if sqlcode < 0 perform error-exit.

       disconnect-connection.
           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           if sqlcode < 0 perform error-exit.

       Error-Exit.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           display "SQL Error: SQLCODE  " SQLCODE of SQLCA.
           display "           SQLSTATE " SQLSTATE of SQLCA.
           display SQLERRMC OF SQLCA.
           accept omitted.
           EXEC SQL DISCONNECT ALL END-EXEC.
           stop run.
