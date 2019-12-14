$Regfile="m328def.dat"
$Crystal=20000000
$hwstack=128
$swstack=256
$framesize=512



'___________ Global constants _____________________________________________________________________


' Configuration parameters
Const ReflexStrikeMinTime = 1000
Const ReflexStrikeRndTime = 3000

Const FeedbackModeSound = 1
Const FeedbackModeLed = 2
Const FeedbackModeBoth = 3
Const FeedbackModeStart = FeedbackModeSound
Const FeedbackModeCount = 3

Const BeepTimeTimer = 300
Const BeepTimeStop = 500
Const BeepTimeStrike = 50
Const BeepTimeBtn = 50
Const BeepTimeBatteryLow = 1000
Const BeepTimeDefaultOff = 100
Const BeepTimeTimeoutOn = 50
Const BeepTimeTimeoutOff = 50

Const BeepOn = 1
Const BeepOff = 0

Const DebounceLimit = 10
Const TimerPeriodInMs = 1
Const TimerReloadValue = 45535

Const LcdNoticeTimeDefault = 1500
Const LcdBackLightTimerStartValue = 30000

' Standard values for I/O handling
Const PinHigh = 1
Const PinLow = 0
Const BtnDown = 1
Const BtnUp = 0


' State machine constants
Const StateReflexFeedbackConfig = 0
Const StateReflexHandSideConfig = 1
Const StateReflexStart = 2
Const StateReflexRun = 3
Const StateDisplayResult = 4

' Battery parameters
Const BatteryCheckInterval = 60000
Const BatteryLevelLow = 720

' Texts
Const TimeUnitText = "ms"


'___________ Global variables _____________________________________________________________________

Dim ActualState As Integer
Dim SelectedFeedbackMode As Integer

Dim FlagLeftButton As Byte                  ' Shows if the MODE button was pressed
Dim FlagRightButton As Byte                   ' Shows if the SET button was pressed
Dim FlagTimer As Byte                       ' Shows if the software timer has expired
Dim FlagBattery As Byte
Dim FlagReflexTimerStarted As Byte          ' Shows if the timer was started when in reflex mode

Dim BtnLeftState As Byte                    ' Stable state of the MODE button (needed for debouncing)
Dim BtnLeftCntr As Byte                     ' Counter value for MODE button (needed for debouncing)
Dim BtnRightState As Byte                   ' Stable state of the SET button (needed for debouncing)
Dim BtnRightCntr As Byte                    ' Counter value for SET button (needed for debouncing)

Dim RefMeasurements(10) As Long             ' Array for holding 10 measurements for average calculation
Dim MeasurementId As Byte                   ' Current array id
Dim PushCount As Byte                       ' Number of strikes

Dim ReflexTime As Dword                     ' Helper variable for storing the current time in reflex mode

Dim CounterTimer As Word                    ' Helper variable for the software timer
Dim CounterBeep As Byte                     ' Helper variable for the beep sound
Dim BeepIntervalOn As Byte                  ' Shows how many timer cycles the beep sound should last
Dim BeepIntervalOff As Byte                 ' Shows how many timer cycles should be between two beep sounds
Dim BeepCount As Byte                       ' Shows how many beep sounds sould be
Dim BeepState As Byte                       ' Helper variable for the beep sound

Dim CurrentTime As Dword                    ' Shows the time elapsed since startup in ms

Dim ActionResult As Integer

Dim PushEnabled As Byte

Dim BatteryTimer As Word
Dim BatteryAdcValue As Word

Dim LcdBackLightTimer as Word

Dim Index As Integer
Dim TempCond As Integer
Dim TempCond2 As Integer
Dim LeftHandedMode as Byte

'___________ Subroutine and function declarations _________________________________________________

Declare Sub LcdUpdate()

Declare Sub EnableLcdBackLight()

Declare Sub BeepMs(ByVal intervalOn As Word)

Declare Sub BeepMsExtended(ByVal intervalOn As Word, ByVal intervalOff As Word, ByVal count As Word)

Declare Sub TimerMs(ByVal timeMs As Word)

Declare Sub LcdNotice(ByVal msg As String, ByVal timeMs As Word)

Declare Function GetFlagLeftButton() As Byte

Declare Function GetFlagRightButton() As Byte

Declare Function GetFlagTimer() As Byte

Declare Function GetFlagBattery() As Byte

'___________ Interrupt config _____________________________________________________________________
Enable Interrupts

Config Timer1 = Timer, Prescale = 1,
On Timer1 TimerISR
TIMER1 = TimerReloadValue
Enable Timer1


'___________ Lcd config ___________________________________________________________________________
Config Lcdpin = Pin , Db4 = Portb.0 , Db5 = Portb.1 , Db6 = Portd.6 , Db7 = Portd.7 , E = Portc.1 , Rs = Portc.0
Config Lcd = 16 * 2
Cursor Off Noblink
Cls

'___________ Adc config ___________________________________________________________________________
Config Adc = Single , Prescaler = Auto , Reference = Internal
Start Adc


'___________ Port config __________________________________________________________________________

Config Pind.2 = Input  'int0 - adc_start

Config Pinb.2 = Input
PinLeftBtn Alias Pinb.2

Config Pind.3 = Input
PinRightBtn Alias Pind.3


Config Pinc.2 = Output
PinLed Alias Portc.2

Config Pinc.3 = Output
PinBeep Alias Portc.3

Config Pinb.5 = Output
PinLcdPower Alias Portb.5

Config Pind.4 = Output
PinLcdBackLight Alias Portd.4


'___________ Initial values _______________________________________________________________________
ActualState = StateReflexFeedbackConfig
SelectedFeedbackMode = FeedbackModeSound

FlagLeftButton = 0
FlagRightButton = 0
FlagTimer = 0
FlagReflexTimerStarted = 0

BtnLeftState = BtnUp
BtnLeftCntr = 0
BtnRightState = BtnUp
BtnRightCntr = 0

MeasurementId = 1
PushCount = 0

ReflexTime = 0

CounterTimer = 0
CounterBeep = 0
BeepCount = 0
BeepIntervalOff = 0
BeepIntervalOn = 0
BeepState = 0

CurrentTime = 0

ActionResult  = 0

PushEnabled = 0

LcdBackLightTimer = LcdBackLightTimerStartValue


'___________ Startup ______________________________________________________________________________

Set PinLed
Set PinBeep
Reset PinLcdBackLight
Reset PinLcdPower

' Show LCD welcome screen
Cls
Home
Lcd " Reflex Trainer "
Waitms 2000

FlagLeftButton = 0
FlagRightButton = 0

Call LcdUpdate()        ' Show the initial display


'___________ Main Program Loop ____________________________________________________________________
Do
    ' Call the action handler of the actual state
    Select Case ActualState
        Case StateReflexFeedbackConfig                                  ' If we are in InitConfig state
            If GetFlagLeftButton() = 1 Then
                If SelectedFeedbackMode = FeedbackModeCount Then    ' Reached the end of the feedback mode list, go back to first mode
                    SelectedFeedbackMode = FeedbackModeStart
                Else
                    Incr SelectedFeedbackMode                       ' Select next feedback mode
                Endif

                Call LcdUpdate()
            Endif

            If GetFlagRightButton() = 1 Then
                ActualState = StateReflexHandSideConfig             ' Go to hand side config state

                Call LcdUpdate()
            Endif

        Case StateReflexHandSideConfig
            If GetFlagLeftButton() = 1 Then
                If LeftHandedMode = 0 Then
                    LeftHandedMode = 1
                Else
                    LeftHandedMode = 0
                Endif

                Call LcdUpdate()
            Endif

            If GetFlagRightButton() = 1 Then
                ActualState = StateReflexStart                      ' Go to reflex start state

                Call LcdUpdate()
            Endif

        Case StateReflexStart                                       ' If we are in Start state
            TempCond = 0
            If GetFlagRightButton() = 1 And LeftHandedMode = 0 Then
                TempCond = 1
            Endif

            If GetFlagLeftButton() = 1 And LeftHandedMode = 1 Then
                TempCond = 1
            Endif

            If TempCond = 1 Then
                MeasurementId = 1
                PushCount = 0
                Call TimerMs(0)
                FlagTimer = 0
                ReflexTime = 0
                FlagReflexTimerStarted = 0
                PushEnabled = 0
                ActualState = StateReflexRun
                Call LcdUpdate()
            Endif

        Case StateReflexRun                                         ' If we are in Run state

            TempCond = 0
            If BtnLeftState = BtnDown And LeftHandedMode = 0 Then
                TempCond = 1
            Endif

            If BtnRightState = BtnDown And LeftHandedMode = 1 Then
                TempCond = 1
            Endif

            If TempCond = 1 And PushEnabled = 1 Then
                PushEnabled = 0

                ReflexTime = CurrentTime - ReflexTime
                FlagReflexTimerStarted = 0
                Call TimerMs(0)

                RefMeasurements(MeasurementId) = ReflexTime
                Incr MeasurementId
                Incr PushCount

                If MeasurementId > 10 Then
                    ActionResult = 0
                    For Index = 1 to 10
                        ActionResult = ActionResult + RefMeasurements(Index)
                    Next Index
                    ActionResult = ActionResult / 10
                    MeasurementId = 1

                    ActualState = StateDisplayResult
                    Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)
                Endif

                Call LcdUpdate()
            Endif

            ' Check if the timer has ended
            If GetFlagTimer() = 1 Then
                If FlagReflexTimerStarted = 1 Then
                    FlagReflexTimerStarted = 2
                    Call TimerMs(999)
                    Call BeepMs(BeepTimeTimer)
                    ReflexTime = CurrentTime
                    PushEnabled = 1

                Elseif FlagReflexTimerStarted = 2 Then
                    PushEnabled = 0
                    FlagReflexTimerStarted = 0

                    RefMeasurements(MeasurementId) = 999
                    Incr MeasurementId
                    Incr PushCount

                    If MeasurementId > 10 Then
                        ActionResult = 0
                        For Index = 1 to 10
                            ActionResult = ActionResult + RefMeasurements(Index)
                        Next Index
                        ActionResult = ActionResult / 10
                        MeasurementId = 1

                        ActualState = StateDisplayResult
                        Call BeepMsExtended(BeepTimeStop, BeepTimeDefaultOff, 2)
                    Else
                        Call BeepMsExtended(BeepTimeTimeoutOn, BeepTimeTimeoutOff, 2)
                    Endif

                    Call LcdUpdate()
                Endif
            Endif

            TempCond = 0
            If BtnRightState = BtnDown And LeftHandedMode = 0 Then
                TempCond = 1
            Endif

            If BtnLeftState = BtnDown And LeftHandedMode = 1 Then
                TempCond = 1
            Endif

            If FlagReflexTimerStarted = 0 And TempCond = 1 Then
                Call TimerMs(Rnd(ReflexStrikeRndTime) + ReflexStrikeMinTime)
                FlagReflexTimerStarted = 1
            Elseif FlagReflexTimerStarted = 1 And TempCond = 0 Then                
                Call TimerMs(Rnd(ReflexStrikeRndTime) + ReflexStrikeMinTime)
            Endif

        Case StateDisplayResult                                     ' If we are in DisplayResult state
            If CounterTimer = 0 Then
                PushEnabled = 0
                Call TimerMs(500)
            Endif
        
            If GetFlagTimer() = 1 Then
                PushEnabled = 1
            Endif
        
            If GetFlagLeftButton() = 1 And PushEnabled = 1 Then
                ActualState = StateReflexStart
                Call LcdUpdate()
            Endif

            If GetFlagRightButton() = 1 And PushEnabled = 1 Then
                If MeasurementId = 11 Then
                    MeasurementId = 1
                Else
                    Incr MeasurementId
                Endif
                Call LcdUpdate()
            Endif
    End Select

    If GetFlagBattery() = 1 Then
        BatteryAdcValue = Getadc(7)
        If BatteryAdcValue < BatteryLevelLow Then
            Call BeepMs(BeepTimeBatteryLow)
            Call LcdNotice("  Low Battery   ", LcdNoticeTimeDefault)
        Endif
    Endif
Loop
End


'___________ Subroutine definitions _______________________________________________________________

Sub LcdUpdate()
    Cls

    Select Case ActualState
        Case StateReflexFeedbackConfig
            Upperline
            Lcd "Feedback setup:"
            Lowerline
            Select Case SelectedFeedbackMode
                Case FeedbackModeSound
                    Lcd "Sound"
                Case FeedbackModeLed
                    Lcd "Blinking"
                Case FeedbackModeBoth
                    Lcd "Sound & Blinking"
            End Select

        Case StateReflexHandSideConfig
            Upperline
            Lcd "Left hand mode:"
            Lowerline
            If LeftHandedMode = 1 Then
                Lcd "ON"
            Else
                Lcd "OFF"
            Endif

        Case StateReflexStart
            Upperline
            If LeftHandedMode = 0 Then
                Lcd "Hold RIGHT btn"
            Else
                Lcd "Hold LEFT btn"
            Endif

            Lowerline
            Lcd "to start"

        Case StateReflexRun
            Upperline
            If PushCount = 0 Then
                Lcd "[1/10]: "
            Else
                Lcd "[" ; PushCount ; "/10]: "
                Lcd RefMeasurements(PushCount) ; TimeUnitText
            Endif

        Case StateDisplayResult
            Upperline
            Lcd "Results:"
            Lowerline
            If MeasurementId > 10 Then
                Lcd "[Avg]: " ; ActionResult ; TimeUnitText
            Else
                Lcd "[" ; MeasurementId ; "/10]: " ; RefMeasurements(MeasurementId) ; TimeUnitText
            Endif

    End Select
End Sub

'___________ Helper functions and subroutines _____________________________________________________

Sub EnableLcdBackLight()
    LcdBackLightTimer = LcdBackLightTimerStartValue
    Reset PinLcdBackLight
End Sub

Sub BeepMs(ByVal intervalOn As Word)
    BeepIntervalOn = intervalOn
    BeepIntervalOff = 0
    BeepCount = 1
    BeepState = BeepOff
End Sub

Sub BeepMsExtended(ByVal intervalOn As Word, ByVal intervalOff As Word, ByVal count As Word)
    BeepIntervalOn = intervalOn
    BeepIntervalOff = intervalOff
    BeepCount = count
    BeepState = BeepOff
End Sub


Sub TimerMs(ByVal timeMs As Word)
    CounterTimer = timeMs / TimerPeriodInMs
End Sub


Sub LcdNotice(ByVal msg As String, ByVal timeMs As Word)
    Cls                             ' Clear the display
    Home                            ' Return home

    Lcd msg                         ' Print the message
    Waitms timeMs                   ' Wait for the given amount of time

    Call LcdUpdate()                ' Restore the original display
End Sub


' These functions return the actual state of the flags and then reset them
Function GetFlagLeftButton() As Byte
    Local FlagState As Byte

    If FlagLeftButton = 1 Then
       FlagState = 1
       FlagLeftButton = 0
    Else
       FlagState = 0
    Endif
    GetFlagLeftButton = FlagState
End Function

Function GetFlagRightButton() As Byte
    Local FlagState As Byte

    If FlagRightButton = 1 Then
       FlagState = 1
       FlagRightButton = 0
    Else
       FlagState = 0
    Endif
    GetFlagRightButton = FlagState
End Function

Function GetFlagTimer() As Byte
    Local FlagState As Byte

    If FlagTimer = 1 Then
       FlagState = 1
       FlagTimer = 0
    Else
       FlagState = 0
    Endif
    GetFlagTimer = FlagState
End Function

Function GetFlagBattery() As Byte
    Local FlagState As Byte

    If FlagBattery = 1 Then
       FlagState = 1
       FlagBattery = 0
    Else
       FlagState = 0
    Endif
    GetFlagBattery = FlagState
End Function


'___________ Timer interrupt service routine ______________________________________________________
TimerISR:

    ' Set up timer to have a 1ms period
    TIMER1 = TimerReloadValue

    ' Increase the current time
    CurrentTime = CurrentTime + 1


    ' Debouncing the LEFT btn
    If BtnLeftState = BtnUp Then                ' Left button state is up
        If PinLeftBtn = PinLow Then             ' If the left button is pressed, increase the debounce cntr
            Incr BtnLeftCntr
        Else                                    ' Else reset the counter to zero
            BtnLeftCntr = 0
        Endif

        If BtnLeftCntr >= DebounceLimit Then    ' Debounce complete, btn is pressed down so set the stable state to down and set btn flag
            BtnLeftCntr = 0
            BtnLeftState = BtnDown
            FlagLeftButton = 1
            Call EnableLcdBackLight()
        Endif
    Else                                        ' Left button state is down
        If PinLeftBtn = PinHigh Then            ' If the left button is released, increase the debounce cntr
            Incr BtnLeftCntr
        Else                                    ' Else reset the counter to zero
            BtnLeftCntr = 0
        Endif

        If BtnLeftCntr >= DebounceLimit Then    ' Debounce complete, btn is released so set the stable state to up
            BtnLeftCntr = 0
            BtnLeftState = BtnUp
        Endif
    Endif


    ' Debouncing the RIGHT btn
    If BtnRightState = BtnUp Then               ' Right button state is up
        If PinRightBtn = PinLow Then            ' If the right button is pressed, increase the debounce cntr
            Incr BtnRightCntr
        Else                                    ' Else reset the counter to zero
            BtnRightCntr = 0
        Endif

        If BtnRightCntr >= DebounceLimit Then   ' Debounce complete, btn is pressed down so set the stable state to down and set btn flag
            BtnRightCntr = 0
            BtnRightState = BtnDown
            FlagRightButton = 1
            Call EnableLcdBackLight()
        Endif
    Else                                        ' Right button state is down
        If PinRightBtn = PinHigh Then           ' If the right button is released, increase the debounce cntr
            Incr BtnRightCntr
        Else                                    ' Else reset the counter to zero
            BtnRightCntr = 0
        Endif

        If BtnRightCntr >= DebounceLimit Then   ' Debounce complete, btn is released so set the stable state to up
            BtnRightCntr = 0
            BtnRightState = BtnUp
        Endif
    Endif


    ' Beep sound
    If BeepCount > 0 And CounterBeep = 0 Then
        If BeepState = BeepOff Then
            CounterBeep = BeepIntervalOn
            BeepState = BeepOn
            Decr BeepCount
        Else
            If BeepIntervalOff > 0 Then
                CounterBeep = BeepIntervalOff
            Endif
            BeepState = BeepOff
        Endif
    Endif

    If CounterBeep > 0 Then
        Decr CounterBeep
        If BeepState = BeepOn Then
            If SelectedFeedbackMode = FeedbackModeSound Then
                Reset PinBeep
            Elseif SelectedFeedbackMode = FeedbackModeLed Then
                Reset PinLed
            Elseif SelectedFeedbackMode = FeedbackModeBoth Then
                Reset PinLed
                Reset PinBeep
            Endif
        Endif

        If CounterBeep = 0 Then
            Set PinLed
            Set PinBeep
        Endif
    Endif


    ' Software Timer
    If CounterTimer > 0 Then
        Decr CounterTimer

        If CounterTimer = 0 Then
            FlagTimer = 1
        Endif
    Endif


    ' Battery Timer
    If BatteryTimer > 0 Then
        Decr BatteryTimer
    Else
        BatteryTimer = BatteryCheckInterval
        FlagBattery = 1
    Endif

    ' Lcd Backlight Timer
    If LcdBackLightTimer > 0 Then
        Decr LcdBackLightTimer
        If LcdBackLightTimer = 0 Then
            Set PinLcdBackLight
        Endif
    Endif
Return
