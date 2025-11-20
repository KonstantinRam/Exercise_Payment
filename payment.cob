       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYER-SALARY-CALCULATOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 C-STANDARD-WORK-WEEK     PIC 9(3) VALUE 40.
       01 C-MAX-HOURS-WEEK         PIC 9(3) VALUE 168.
       01 C-HOURS-WARNING          PIC 9(3) VALUE 100.

       01 C-ERROR-HOURS-OVERFLOW   PIC X(120) VALUE "Somehow this person
      -   " this person worked more hours than week holds. You were repo
      -	  "rted to time police!".

       01 C-WARNING-HOURS-OVERFLOW PIC X(120) VALUE "Work hours over res
      -   "onable limit. Please, be sure it's not an error.".

      *> Variables for data accuired during runtime.
       01 WS-EMPLOYEE-NAME     PIC X(30).
       01 WS-HOURS             PIC 999V9 VALUE 0.
      *> Hours Calculations
       01 WS-HOURS-CALCULATED.
            05 WS-HOURS-REGULAR     PIC 99V9 VALUE 0.
            05 WS-HOURS-OVERTIME    PIC 99V9 VALUE 0.

      *> Input string for name or command (QUIT).
       01 WS-STRING-INPUT      PIC X(50) VALUE SPACES.
       01 WS-STRING-INPUT-SIZE PIC 99 VALUE 0.
       
      *> Pay rates.
       01 WS-PAY-RATE-HOUR-BASE PIC 99V99 VALUE 25.
       01 WS-PAY-RATE-HOUR-OT   PIC 99V99.
       01 WS-OT-MODIFIER        PIC 9V9 VALUE 1.5.

      *> Payment Calculations
       01 WS-PAYMENT-CALCULATED.
            05 WS-PAYMENT-REG      PIC 9(8)V99 VALUE 0.
            05 WS-PAYMENT-OT       PIC 9(8)V99 VALUE 0.
            05 WS-PAYMENT-TOTAL    PIC 9(8)V99 VALUE 0.

      *> Display strings.
       01 WS-PAYMENT-DISP.
            05 WS-EMPLOYEE-NAME-DSP      PIC X(40) VALUE SPACES.
            05 WS-PAYMENT-REG-DSP        PIC $$$,$$9.99 VALUE 0.
            05 WS-PAYMENT-OT-DSP         PIC $$$,$$9.99 VALUE 0.
            05 WS-PAYMENT-TOTAL-DSP      PIC $$$,$$9.99 VALUE 0.
            05 WS-PAY-RATE-HOUR-BASE-DSP PIC $$$,$$9.99 VALUE 0.
            05 WS-PAY-RATE-HOUR-OT-DSP   PIC $$$,$$9.99 VALUE 0.
            05 WS-HOURS-REGULAR-DSP      PIC Z9.9 VALUE 0.
            05 WS-HOURS-OVERTIME-DSP     PIC Z9.9 VALUE 0.
            05 WS-HOURS-TOTAL-DSP        PIC Z9.9 VALUE 0.
       
       01 WS-DISPLAY-STRING    PIC X(100).
       
      *> Validation variables.
       01 VALIDATION.
            05 VN-VALID-STRING-INPUT PIC X VALUE 'N'.
                88 STRING-VALID      VALUE 'Y'.
                88 STRING-INVALID    VALUE 'N'.
        05 VN-REQUESTS           PIC X VALUE 'N'.
            88 REQUEST-QUIT      VALUE 'Q'.
        05 VN-VALID-HOURS-INPUT  PIC X VALUE 'N'.
            88 HOURS-VALID       VALUE 'Y'.
            88 HOURS-INVALID     VALUE 'N'.

       PROCEDURE DIVISION.
        PERFORM UNTIL REQUEST-QUIT 
        
            PERFORM GET-STRING-INPUT

            IF REQUEST-QUIT
                EXIT PERFORM
            END-IF

      *>    Employees may have different rates, we need to recalculate OT and other rates every time
      *>    Not applicable in this case, but we think about it.
            PERFORM CALCULATE-RATES

            PERFORM GET-HOURS-WORKED
            PERFORM CALCULATE-HOURS
            PERFORM CALCULATE-PAYMENT
            PERFORM DISPLAY-RESULTS
        END-PERFORM

        DISPLAY "Execution stopped."
        STOP RUN
        GOBACK.
      
        DISPLAY-RESULTS.
            INITIALIZE WS-PAYMENT-DISP

            MOVE WS-PAYMENT-REG TO WS-PAYMENT-REG-DSP
            MOVE WS-PAYMENT-OT TO WS-PAYMENT-OT-DSP
            MOVE WS-PAY-RATE-HOUR-BASE TO WS-PAY-RATE-HOUR-BASE-DSP
            MOVE WS-PAY-RATE-HOUR-OT TO WS-PAY-RATE-HOUR-OT-DSP
            MOVE WS-HOURS-REGULAR TO WS-HOURS-REGULAR-DSP
            MOVE WS-HOURS-OVERTIME TO WS-HOURS-OVERTIME-DSP
            MOVE WS-HOURS TO WS-HOURS-TOTAL-DSP
            MOVE WS-PAYMENT-TOTAL TO WS-PAYMENT-TOTAL-DSP
            MOVE WS-EMPLOYEE-NAME TO WS-EMPLOYEE-NAME-DSP

            DISPLAY "Employee: " WS-EMPLOYEE-NAME-DSP

            STRING "Regular:  " DELIMITED BY SIZE
                WS-HOURS-REGULAR-DSP DELIMITED BY SIZE
                " hrs @" DELIMITED BY SIZE
                WS-PAY-RATE-HOUR-BASE-DSP DELIMITED BY SIZE
                " = " DELIMITED BY SIZE
                WS-PAYMENT-REG-DSP DELIMITED BY SIZE
                INTO WS-DISPLAY-STRING
            END-STRING
            DISPLAY WS-DISPLAY-STRING

            IF WS-PAYMENT-OT = ZERO
                MOVE "No overtime this week." TO WS-DISPLAY-STRING
            ELSE
                STRING "Overtime: " DELIMITED BY SIZE
                    WS-HOURS-OVERTIME-DSP DELIMITED BY SIZE
                    " hrs @" DELIMITED BY SIZE
                    WS-PAY-RATE-HOUR-OT-DSP DELIMITED BY SIZE
                    " = " DELIMITED BY SIZE
                    WS-PAYMENT-OT-DSP DELIMITED BY SIZE
                    INTO WS-DISPLAY-STRING
                END-STRING
            END-IF
            DISPLAY WS-DISPLAY-STRING

            DISPLAY "Total Pay: " WS-PAYMENT-TOTAL-DSP
            DISPLAY " "
        .

        CALCULATE-RATES.
            COMPUTE WS-PAY-RATE-HOUR-OT = 
                    WS-PAY-RATE-HOUR-BASE * WS-OT-MODIFIER
        .
      
        CALCULATE-PAYMENT.
            INITIALIZE WS-PAYMENT-CALCULATED

            MULTIPLY WS-HOURS-REGULAR
            BY WS-PAY-RATE-HOUR-BASE
            GIVING WS-PAYMENT-REG
            END-MULTIPLY

            MULTIPLY WS-HOURS-OVERTIME
                BY WS-PAY-RATE-HOUR-OT
                GIVING WS-PAYMENT-OT
            END-MULTIPLY

            COMPUTE WS-PAYMENT-TOTAL = WS-PAYMENT-REG + WS-PAYMENT-OT
        .

        CALCULATE-HOURS.
            INITIALIZE WS-HOURS-CALCULATED

            IF WS-HOURS <= C-STANDARD-WORK-WEEK
                MOVE WS-HOURS TO WS-HOURS-REGULAR
                EXIT PARAGRAPH
            END-IF

            MOVE C-STANDARD-WORK-WEEK TO WS-HOURS-REGULAR
            SUBTRACT C-STANDARD-WORK-WEEK
                FROM WS-HOURS
                GIVING WS-HOURS-OVERTIME
            END-SUBTRACT
        .

        GET-HOURS-WORKED.
            INITIALIZE WS-HOURS
            INITIALIZE VN-VALID-HOURS-INPUT
            PERFORM UNTIL HOURS-VALID
                DISPLAY "Enter hours worked this week:" 
                    WITH NO ADVANCING
                ACCEPT WS-HOURS
                IF WS-HOURS > C-MAX-HOURS-WEEK
                    DISPLAY C-ERROR-HOURS-OVERFLOW
                ELSE
                    IF WS-HOURS > C-HOURS-WARNING
                        DISPLAY C-WARNING-HOURS-OVERFLOW
                    END-IF

                    IF WS-HOURS >= 0
                        SET HOURS-VALID TO TRUE
                    END-IF
                END-IF
            END-PERFORM
        .

        GET-STRING-INPUT.
            INITIALIZE VN-VALID-STRING-INPUT
            INITIALIZE WS-STRING-INPUT

            PERFORM UNTIL STRING-VALID OR REQUEST-QUIT
                DISPLAY "Enter name (or QUIT)" WITH NO ADVANCING
                ACCEPT WS-STRING-INPUT
                
                MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-STRING-INPUT)) 
                    TO WS-STRING-INPUT-SIZE
                
                IF WS-STRING-INPUT-SIZE > LENGTH OF WS-EMPLOYEE-NAME
                    DISPLAY "Input exceeded allowed lenght."
                ELSE
                    SET STRING-VALID TO TRUE
                END-IF

                IF WS-STRING-INPUT = 'QUIT'
                    SET REQUEST-QUIT TO TRUE
                ELSE
                    MOVE WS-STRING-INPUT TO WS-EMPLOYEE-NAME
                END-IF
            END-PERFORM
        .
