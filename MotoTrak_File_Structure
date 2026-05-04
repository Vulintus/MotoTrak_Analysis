# MotoTrak Session Data File Format (*.MOTOTRAK)

**Version: 1**

*   Header:
    *   (int8) - File format version.
    *   (float64) - Session start time (serial date number).
    *   (float64) - Session end time (serial date number).
    *   (uint8) - Number of characters in the rat’s name.
        *   (uchar) - Characters of the rat’s name.
    *   (uint8) - Number of characters in the booth title.
        *   (uchar) - Characters of the booth title.
    *   (uint8) - Number of characters in the stage title.
        *   (uchar) - Characters of the stage title.
    *   (uint8) - Number of characters in the device description.
        *   (uchar) - Characters of the device description.
    *   (uint16) - Number of characters in the notes.
        *   (uchar) - Characters of the notes.
    *   (uint8) - Number of coefficients in the calibration function
        *   (float32) - Coefficient values for the calibration function.
    *   (uint8) - Number of stream signals
        *   (uint8) - Number of characters in the structure field name for the stream signal.
            *   (uchar) - Characters of the structure field name.
        *   (uint8) - Number of characters in the stream signal units description.
            *   (uchar) - Characters of the stream signal units description.
    *   (uint) - Number of constant value parameters.
        *   (uint8) - Number of characters in the structure field name for the constant parameter.
            *   (uchar) - Characters of the structure field name.
        *   (float32) - Constant parameter value.
    *   (uint) - Number of constant string parameters.
        *   (uint8) - Number of characters in the structure field name for the constant parameter.
            *   (uchar) - Characters of the structure field name.
        *   (uint8) - Number of characters in the constant parameter string.
            *   (uchar) - Characters of the constant parameter string.
    *   (uint) - Number of variable value parameters.
        *   (uint8) - Number of characters in the structure field name for the variable parameter.
            *   (uchar) - Characters of the structure field name.
*   Repeating once per trial:
    *   (uint32) - Trial number
        *   A trial number of zero indicates a manual feeding or pause.
    *   (float64) - Start time of trial (serial date number).
    *   (uchar) - Outcome code.
        *   H (72) = Hit
        *   M (77) = Miss
        *   F (70) = Manual Feed
        *   P (80) = Pause
        *   If the outcome was a pause…
            *   (float64) - End time of pause (serial date number).
    *   (float32) - Response window (in seconds).
    *   (float32) - Pre-trial sampling window (in seconds).
    *   (float32) - Post-trial sampling window (in seconds).
    *   (float32) - Post-trial time-out (in seconds).
    *   (float32) - Manipulandum position (in centimeters).
    *   (float32) - Trial initiation threshold.
    *   (float32) - Threshold minimum for hit/reward.
    *   (float32) - Threshold maximum for hit/reward.
    *   (uint8) - Number of hits/rewards.
        *   (float64) - Hit/reward times (serial date number).
    *   (uint8) - Number of output trigger events.
        *   (float64) - output trigger event times (serial date number).
    *   Iterate through number of variable value parameters…
        *   (float32) - Variable parameter value.
    *   (uint32) - Number of samples in the stream signals.
        *   (int32) - Signal timepoints (in microseconds).
        *   Iterate through the number of stream signals…
            *   (float32) - Calibrated stream values.

---

# Legacy MotoTrak Session Data File Format (*.ARDYMOTOR)

*(Changes from previous versions are noted using strikethrough for removed text.)*

## Version: -3 (incremental change from version -1, not version -2)

*   (int8) - Format version.
*   (uint16) - DayCode.
*   (uint8) - Booth number.
*   (uint8) - Number of characters in the rat's name.
    *   (uchar) - Characters of the rat's name.
*   (float32) - Input device position (in centimeters).
*   (uint8) - Number of characters in the stage title.
    *   (uchar) - Characters of the stage title.
*   (uint8) - Number of characters in the device description.
    *   (uchar) - Characters of the device description.
    *   If the device is the pull, knob, or lever:
        *   (float32) - Coefficients of the calibration equation (m,b).
    *   If the device is the wheel~~ or the knob~~:
        *   (float32) - Number of degrees per tick.
*   (uint8) - Number of characters in the constraint description.
    *   (uchar) - Characters of the constraint description.
*   (uint8) - Number of characters in the threshold units.
    *   (uchar) - Characters of the threshold units.
*   Repeat through all trials:
    *   (uint32) - Trial Number.
        *   A trial number of zero indicates a manual feeding or pause.
    *   (float64) - Start time of trial (serial date number).
    *   (uint8) - Outcome
        *   H (72) = Hit
        *   M (77) = Miss
        *   F (70) = Manual Feed
        *   P (80) = Pause
        *   If the outcome was a pause:
            *   (float64) - End time of the pause.
    *   (float32) - Response window (in seconds).
    *   (float32) - Threshold for trial initiation.
    *   (float32) - Threshold for reward.
    *   (uint8) - Number of hits/rewards.
        *   (float64) - Hit/reward times (serial date number).
    *   (uint8) - Number of VNS events.
        *   (float64) - VNS event times (serial date number).
    *   (uint32) - Number of samples in the sensor signal.
    *   (int16) - Signal timepoints (in microseconds).
    *   (float32) - Device signal values.
    *   (int16) - IR signal values.

## Version: -2

*   (int16) - Format version.
*   ~~(uint16) - DayCode.~~
*   (uint16) - Booth number.
*   (uint8) - Number of characters in the rat's name.
    *   (uchar) - Characters of the rat's name.
*   (float32) - Input device position (in centimeters).
*   (uint8) - Number of characters in the stage title.
    *   (uchar) - Characters of the stage title.
*   (uint8) - Number of characters in the device description.
    *   (uchar) - Characters of the device description.
    *   If the device is the pull:
        *   (float32) - Coefficients of the calibration equation (m,b).
    *   If the device is the wheel or the knob:
        *   (float32) - Number of degrees per tick.
*   (uint8) - Number of characters in the constraint description.
    *   (uchar) - Characters of the constraint description.
*   (uint8) - Number of characters in the threshold units.
    *   (uchar) - Characters of the threshold units.
*   (float32) - Pre-trial sampling duration (in milliseconds).
*   Repeat through all trials:
    *   (uint32) - Trial Number.
        *   A trial number of zero indicates a manual feeding or pause.
    *   (float64) - Start time of trial (serial date number).
    *   (uint8) - Outcome
        *   H (72) = Hit
        *   M (77) = Miss
        *   F (70) = Manual Feed
        *   P (80) = Pause
        *   If the outcome was a pause:
            *   (float64) - End time of the pause.
    *   (float32) - Response window (in seconds).
    *   (float32) - Threshold for trial initiation.
    *   (float32) - Threshold for reward.
    *   (uint8) - Number of hits/rewards.
        *   (float64) - Hit/reward times (serial date number).
    *   (uint8) - Number of VNS events.
        *   (float64) - VNS event times (serial date number).
    *   (uint32) - Number of samples in the sensor signal.
    *   (uint16) - Signal timepoints (in microseconds).
    *   (float32) - Device signal values.
    *   (int16) - IR signal values.

## Version: -1

*   (int8) - Format version.
*   (uint16) - DayCode.
*   (uint8) - Booth number.
*   (uint8) - Number of characters in the rat's name.
    *   (uchar) - Characters of the rat's name.
*   (float32) - Input device position (in centimeters).
*   (uint8) - Number of characters in the stage title.
    *   (uchar) - Characters of the stage title.
*   (uint8) - Number of characters in the device description.
    *   (uchar) - Characters of the device description.
    *   If the device is the pull:
        *   (float32) - Coefficients of the calibration equation (m,b).
    *   If the device is the wheel or the knob:
        *   (float32) - Number of degrees per tick.
*   (uint8) - Number of characters in the constraint description.
    *   (uchar) - Characters of the constraint description.
*   (uint8) - Number of characters in the threshold units.
    *   (uchar) - Characters of the threshold units.
*   Repeat through all trials:
    *   (uint32) - Trial Number.
        *   A trial number of zero indicates a manual feeding or pause.
    *   (float64) - Start time of trial (serial date number).
    *   (uint8) - Outcome
        *   H (72) = Hit
        *   M (77) = Miss
        *   F (70) = Manual Feed
        *   P (80) = Pause
        *   If the outcome was a pause:
            *   (float64) - End time of the pause.
    *   (float32) - Response window (in seconds).
    *   (float32) - Threshold for trial initiation.
    *   (float32) - Threshold for reward.
    *   (uint8) - Number of hits/rewards.
        *   (float64) - Hit/reward times (serial date number).
    *   (uint8) - Number of VNS events.
        *   (float64) - VNS event times (serial date number).
    *   (uint32) - Number of samples in the sensor signal.
    *   (int16) - Signal timepoints (in microseconds).
    *   (float32) - Device signal values.
    *   (int16) - IR signal values.

## Version: Original

*   (uint16) - DayCode.
*   (uint8) - Booth number.
*   (uint8) - Number of characters in the rat's name.
    *   (uchar) - Characters of the rat's name.
*   (uint8) - Input device position (in inches).
*   (uint8) - Response window (in seconds).
*   (uchar) - Characters of the stage title.
*   (uchar) - Characters of the device description.
*   (uint8) - Bin size.
*   (uint8) - Number of stimulus parameters.
    *   (int16) - Number of characters in the name of parameter \#1.
    *   (uchar) - Characters of parameter \#1 name.
    *   …
*   Repeat through all trials:
    *   (uint16) - Threshold for reward.
    *   (float64) - Start time of trial (serial date number).
    *   (float64) - Hit/reward time (serial date number).
    *   (float64) - Trial Outcome (0 = miss, 1 = hit w/ no stim., 2 = hit w/ stim.)
    *   (uint32) - Number of beam breaks on IR\#1
        *   (float64) - Beam break times (serial date number).
    *   (uint32) - Number of beam breaks on IR\#2
        *   (float64) - Beam break times (serial date number).
    *   (uint32) - Number of beam breaks on IR\#3
        *   (float64) - Beam break times (serial date number).
    *   (uint32) - Number of samples in the sensor signal.
        *   (float64) - Signal values.
    *   ...
