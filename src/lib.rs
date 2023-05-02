#![warn(missing_docs)]

//! Micro-tuning format parsing and frequency finding library.
//!
//! This library provides parsing of SCL (scale) and KBM (keyboard mapping) files,
//! constructing tunings from scales and keyboard mappings and finding frequencies of notes.
//!
//! This library is mostly a direct rewrite of Surge Synthesizer Teams's [`tuning-library`]. Even
//! the documentation is taken almost word for word.
//!
//! [`tuning-library`]: https://github.com/surge-synthesizer/tuning-library

use std::{error::Error, fmt::Display, fs};

/// Frequency of a MIDI note 0. Equal to `440 * 2^(69/12)`.
pub const MIDI_0_FREQ: f64 = 8.17579891564371;

/// Errors
#[derive(Debug)]
pub enum TuningError {
    /// Error parsing an SCL/KBM file/string.
    ParseError(String),
    /// Tone is a ratio with either denominator or numerator equal to 0.
    InvalidTone,
    /// Number of notes is less then 1 or not equal to listed notes.
    InvalidNoteCount,
    /// Scale contains no notes.
    TooFewNotes,
    /// Tuning attemped to tune unmapped key.
    TuningUnmappedKey,
    /// Mapping is longer than scale.
    MappingLongerThanScale,
    /// Error reading the file.
    FileError,
    /// Cannot divide zero span.
    ZeroSpan,
    /// Cannot divide non-positive cents amount.
    NonPositiveCents,
    /// Cannot divide into zero steps.
    ZeroSteps,
    /// Incomplete KBM file.
    IncompleteKBM,
    /// Incomplete SCL file.
    IncompleteSCL,
    /// Given amount of keys is not equal to number of listed keys.
    InvalidKeyCount,
}

impl Display for TuningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TuningError::ParseError(error) => write!(f, "Error parsing the file: {error}"),
            TuningError::InvalidTone => write!(f, "Value is an invalid tone."),
            TuningError::InvalidNoteCount => write!(f, "Invalid number of notes."),
            TuningError::TooFewNotes => write!(f, "Too few notes."),
            TuningError::TuningUnmappedKey => write!(f, "Attempted to tune unmapped key."),
            TuningError::MappingLongerThanScale => {
                write!(f, "Keyboard mapping is longer than the scale.")
            }
            TuningError::FileError => write!(f, "Error reading the file."),
            TuningError::ZeroSpan => write!(f, "Cannot divide zero span."),
            TuningError::NonPositiveCents => {
                write!(f, "Cannot divide by non-positive cents amount.")
            }
            TuningError::ZeroSteps => write!(f, "Cannot divide by zero steps."),
            TuningError::IncompleteKBM => write!(f, "KBM file is incomplete."),
            TuningError::IncompleteSCL => write!(f, "SCL file is incomplete."),
            TuningError::InvalidKeyCount => write!(f, "Invalid number of keys."),
        }
    }
}

impl Error for TuningError {}

#[doc(hidden)]
pub struct AllowTuningOnUnmapped(pub bool);

/// Value of a tone.
///
/// The value of a tone is given either as a cents value or ratio.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ToneValue {
    /// Value of a tone given as cents value.
    /// ```
    /// # use tuning_library_rs::*;
    /// ToneValue::Cents(1200.0); // is an octave
    /// ToneValue::Cents(700.0); // is an 12-EDO fifth
    /// ```
    Cents(f64),

    /// Value of a tone given as a ratio.
    /// ```
    /// # use tuning_library_rs::*;
    /// ToneValue::Ratio(2, 1); // is an octave as well
    /// ToneValue::Ratio(3, 2); // is a JI fifth
    /// ```
    Ratio(i64, i64),
}

impl ToneValue {
    fn cents(&self) -> f64 {
        match self {
            Self::Cents(value) => *value,
            Self::Ratio(n, d) => 1200.0 * (*n as f64 / *d as f64).log2() / 2f64.log2(),
        }
    }

    fn float_value(&self) -> f64 {
        self.cents() / 1200.0 + 1.0
    }
}

impl Default for ToneValue {
    fn default() -> Self {
        ToneValue::Ratio(1, 1)
    }
}

impl Eq for ToneValue {}

impl PartialOrd for ToneValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.cents().partial_cmp(&other.cents())
    }
}

impl Ord for ToneValue {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Display for ToneValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ToneValue::Cents(value) => write!(f, "{value}c"),
            ToneValue::Ratio(n, d) => write!(f, "{n}/{d}"),
        }
    }
}

/// A Tone is a single entry in a SCL file. It is expressed either in cents or in a ratio, as
/// described in the SCL documentation.
///
/// In most normal use, you will not use this struct, and it will be internal to a [`Scale`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tone {
    /// Value of the tone.
    pub value: ToneValue,

    /// String representation of the tone. Is set if parsed from string.
    pub string_rep: String,

    /// Number of line on which the tone was specified. Is set if parsed from file.
    pub lineno: Option<usize>,
}

impl Tone {
    /// Default constructor. Initializes tone with ratio `1/1` (or 0 cents).
    pub fn new() -> Self {
        Tone::default()
    }

    /// Returns cents value of a tone.
    pub fn cents(&self) -> f64 {
        self.value.cents()
    }

    /// Returns float value of a tone. Equal to `cents() / 1200 + 1`.
    pub fn float_value(&self) -> f64 {
        self.value.float_value()
    }

    /// Constructs a tone from a string.
    ///
    /// Returns a `TuningError::ParseError` if `line` is not a valid tone representation.
    ///
    /// Returns a `TuningError::InvalidTone` if `line` is a ratio with either denominator or
    /// numerator equal to 0.
    ///
    /// Returns an `Ok` variant for valid tones.
    pub fn from_string(line: &str, lineno: Option<usize>) -> Result<Self, TuningError> {
        let mut t = Tone::new();
        t.string_rep = line.to_string();
        t.lineno = lineno;

        if line.find('.').is_some() {
            t.value = ToneValue::Cents(match line.parse() {
                Ok(x) => x,
                Err(_) => {
                    return Err(TuningError::ParseError(format!(
                        "Line {} contains . but is not numeric.",
                        lineno.unwrap_or_default()
                    )))
                }
            });

            return Ok(t);
        }

        let parts: Vec<&str> = line.split('/').collect();
        match parts[..] {
            [one] => {
                t.value = ToneValue::Ratio(
                    match one.parse() {
                        Ok(x) => x,
                        Err(_) => {
                            return Err(TuningError::ParseError(format!(
                                "Numerator on line {} is not numeric.",
                                lineno.unwrap_or_default()
                            )))
                        }
                    },
                    1,
                )
            }
            [one, two] => {
                t.value = ToneValue::Ratio(
                    match one.parse() {
                        Ok(x) => x,
                        Err(_) => {
                            return Err(TuningError::ParseError(format!(
                                "Numerator on line {} is not numeric.",
                                lineno.unwrap_or_default()
                            )))
                        }
                    },
                    match two.parse() {
                        Ok(x) => x,
                        Err(_) => {
                            return Err(TuningError::ParseError(format!(
                                "Denominator on line {} is not numeric.",
                                lineno.unwrap_or_default()
                            )))
                        }
                    },
                )
            }
            _ => {
                return Err(TuningError::ParseError(format!(
                    "Value on line {} is not a valid fraction.",
                    lineno.unwrap_or_default()
                )))
            }
        }

        if let ToneValue::Ratio(d, n) = t.value {
            if d == 0 || n == 0 {
                return Err(TuningError::InvalidTone);
            }
        }

        Ok(t)
    }
}

impl Display for Tone {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string_rep)
    }
}

impl Default for Tone {
    fn default() -> Self {
        Tone {
            value: ToneValue::default(),
            string_rep: String::from("1/1"),
            lineno: None,
        }
    }
}

/// The Scale is the representation of the SCL file.
///
/// It contain several key features. Most importantly it has a [`Scale::count()`] and a [vector of
/// Tones][`Scale::tones`].
///
/// In most normal use, you will simply pass around instances of this struct to a [`Tuning`], but in
/// some cases you may want to create or inspect this struct yourself. Especially if you are
/// displaying this struct to your end users, you may want to use the [`Scale::raw_text`] or
/// [`Scale::count()`] methods.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Default, Clone, Debug)]
pub struct Scale {
    /// The name in the SCL file. Informational only.
    pub name: String,

    /// The description in the SCL file. Informational only.
    pub description: String,

    /// The raw text of the SCL file used to create this Scale.
    pub raw_text: String,

    /// The number of tones.
    count: usize,

    /// The tones.
    pub tones: Vec<Tone>,
}

impl Scale {
    /// Constructs an empty scale.
    pub fn new() -> Self {
        Scale {
            name: String::from("empty scale"),
            ..Default::default()
        }
    }

    /// Number of tones in the scale.
    pub fn count(&self) -> usize {
        self.count
    }

    /// Returns a Scale or an error from the SCL file contents in `fname`.
    pub fn read_scl_file<P>(fname: P) -> Result<Self, TuningError>
    where
        P: AsRef<std::path::Path>,
    {
        let content = match fs::read_to_string(fname) {
            Ok(lines) => lines,
            Err(_) => return Err(TuningError::FileError),
        };

        Scale::parse_scl_data(&content)
    }

    /// Returns a Scale or an error from the SCL file contents in memory.
    pub fn parse_scl_data(scl_contents: &str) -> Result<Self, TuningError> {
        enum State {
            ReadHeader,
            ReadCount,
            ReadNote,
            Trailing,
        }
        let mut state = State::ReadHeader;

        let mut res = Scale::new();

        for (lineno, line) in scl_contents.split('\n').map(|x| x.trim()).enumerate() {
            if (matches!(state, State::ReadNote) && line.is_empty()) || line.starts_with('!') {
                continue;
            }

            match state {
                State::ReadHeader => {
                    res.description = line.to_string();
                    state = State::ReadCount
                }
                State::ReadCount => {
                    res.count = match line.parse() {
                        Ok(value) if value >= 1 => value,
                        Ok(_) => return Err(TuningError::InvalidNoteCount),
                        Err(_) => {
                            return Err(TuningError::ParseError(format!(
                                "Error parsing note count on line {lineno}."
                            )))
                        }
                    };

                    state = State::ReadNote;
                }
                State::ReadNote => {
                    let t = Tone::from_string(line, Some(lineno))?;
                    res.tones.push(t);

                    if res.tones.len() == res.count {
                        state = State::Trailing;
                    }
                }
                State::Trailing => (),
            }
        }

        if !matches!(state, State::ReadNote | State::Trailing) {
            return Err(TuningError::IncompleteSCL);
        }

        if res.tones.len() != res.count {
            return Err(TuningError::InvalidNoteCount);
        }

        res.raw_text = scl_contents.to_string();
        Ok(res)
    }

    /// Provides a utility scale which is the "standard tuning" scale.
    pub fn even_temperament_12_note_scale() -> Self {
        let data = "! 12 Tone Equal Temperament.scl
            !
            12 Tone Equal Temperament | ED2-12 - Equal division of harmonic 2 into 12 parts
             12
            !
             100.00000
             200.00000
             300.00000
             400.00000
             500.00000
             600.00000
             700.00000
             800.00000
             900.00000
             1000.00000
             1100.00000
             2/1";

        Scale::parse_scl_data(data).expect("This shouldn't fail")
    }

    /// Provides a scale refered to as "ED2-17" or "ED3-24" by dividing the `span` into `m` points.
    /// `Scale::even_division_of_span_by_m(2, 12)` should be the
    /// [`Scale::even_temperament_12_note_scale()`].
    ///
    /// ```
    /// # use tuning_library_rs::Scale;
    /// let s1 = Scale::even_division_of_span_by_m(2, 12).unwrap(); // equal to `even_temperament_12_note_scale()`
    /// let s2 = Scale::even_division_of_span_by_m(2, 17).unwrap(); // ED2-17
    /// let s3 = Scale::even_division_of_span_by_m(3, 24).unwrap(); // ED3-24
    /// ```
    pub fn even_division_of_span_by_m(span: u32, m: u32) -> Result<Self, TuningError> {
        if span == 0 {
            return Err(TuningError::ZeroSpan);
        }

        if m == 0 {
            return Err(TuningError::ZeroSteps);
        }

        let mut data = String::new();
        data += &format!("! Automatically generated ED{span}-{m} scale\n");
        data += &format!("Automatically generated ED{span}-{m} scale\n");
        data += &format!("{m}\n");
        data += "!\n";

        let top_cents = 1200.0 * (span as f64).log2() / 2f64.log2();
        let d_cents = top_cents / m as f64;
        data += &(1..m)
            .map(|i| format!("{:.32}\n", d_cents * i as f64))
            .collect::<String>();
        data += &format!("{span}/1\n");

        Scale::parse_scl_data(&data)
    }

    /// Provides a scale which divides `cents` into `m` steps. It is less frequently used than
    /// [`Scale::even_division_of_span_by_m()`] for obvious reasons. If you want the last cents
    /// label labeled differently than the cents argument, pass in the associated label.
    pub fn even_division_of_cents_by_m(
        cents: f64,
        m: u32,
        last_label: &str,
    ) -> Result<Self, TuningError> {
        if cents <= 0.0 {
            return Err(TuningError::NonPositiveCents);
        }

        if m == 0 {
            return Err(TuningError::ZeroSteps);
        }

        let mut data = String::new();
        data += &format!("! Automatically generated Even Division of {cents} ct into {m} scale\n");
        data += &format!("Automatically generated Even Division of {cents} ct into {m} scale\n");
        data += &format!("{m}\n");
        data += "!\n";

        let top_cents = cents;
        let d_cents = top_cents / m as f64;
        data += &(1..m)
            .map(|i| format!("{}\n", d_cents * i as f64))
            .collect::<String>();

        data += &match last_label {
            "" => format!("{cents}\n"),
            label => format!("{label}\n"),
        };

        Scale::parse_scl_data(&data)
    }
}

/// The KeyboardMapping struct represents a KBM file.
///
/// In most cases, the salient features are the [`KeyboardMapping::tuning_constant_note`] and
/// [`KeyboardMapping::tuning_frequency`], which allow you to pick a fixed note in the MIDI keyboard
/// when retuning. The KBM file can also remap individual keys to individual points in a scale,
/// which here is done with the [keys vector][`KeyboardMapping::keys`].
///
/// Just as with [`Scale`] the [`KeyboardMapping::raw_text`] member contains the text of the KBM
/// file used.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Default, Clone, Debug)]
pub struct KeyboardMapping {
    /// Size of the mapping.
    count: usize,

    /// First MIDI note to be mapped.
    pub first_midi: i32,

    /// Last MIDI note to be mapped.
    pub last_midi: i32,

    /// Middle MIDI note.
    pub middle_note: i32,

    /// MIDI note to be tuned.
    pub tuning_constant_note: i32,

    /// Frequency of the tuned note.
    pub tuning_frequency: f64,

    /// Pitch of the tuned note. Equal to `tuning_frequency / MIDI_0_FREQ`.
    pub tuning_pitch: f64,

    /// Number of octave degrees.
    pub octave_degrees: i32,

    /// Mapped keys. Rather than an 'x', we use a '-1' for skipped notes (this should use Option or
    /// similar).
    pub keys: Vec<i32>,

    /// Raw text of the KBM file.
    pub raw_text: String,

    /// Name of the mapping.
    pub name: String,
}

impl KeyboardMapping {
    /// Constructs a default `KeyboardMapping`.
    pub fn new() -> Self {
        let mut k = KeyboardMapping {
            count: 0,
            first_midi: 0,
            last_midi: 127,
            middle_note: 60,
            tuning_constant_note: 60,
            tuning_frequency: MIDI_0_FREQ * 32.0,
            tuning_pitch: 32.0,
            octave_degrees: 0,
            raw_text: String::new(),
            ..Default::default()
        };

        k.raw_text = format!(
            "! Default KBM file\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
            k.count,
            k.first_midi,
            k.last_midi,
            k.middle_note,
            k.tuning_constant_note,
            k.tuning_frequency,
            k.octave_degrees
        );

        k
    }

    /// Number of keys in the mapping.
    pub fn count(&self) -> usize {
        self.count
    }

    /// Returns a KeyboardMapping or an error from a KBM file in `fname`.
    pub fn read_kbm_file<P>(fname: P) -> Result<Self, TuningError>
    where
        P: AsRef<std::path::Path>,
    {
        let content = match fs::read_to_string(&fname) {
            Ok(lines) => lines,
            Err(_) => return Err(TuningError::FileError),
        };

        let mut res = KeyboardMapping::parse_kbm_data(&content)?;
        res.name = fname.as_ref().to_str().unwrap_or_default().to_string();
        Ok(res)
    }

    /// Returns a KeyboardMapping or an error from a KBM data in memory.
    pub fn parse_kbm_data(kbm_contents: &str) -> Result<Self, TuningError> {
        enum ParsePosition {
            MapSize,
            FirstMidi,
            LastMidi,
            Middle,
            Reference,
            Freq,
            Degree,
            Keys,
            Trailing,
        }

        impl ParsePosition {
            fn next(&self) -> ParsePosition {
                match self {
                    ParsePosition::MapSize => Self::FirstMidi,
                    ParsePosition::FirstMidi => Self::LastMidi,
                    ParsePosition::LastMidi => Self::Middle,
                    ParsePosition::Middle => Self::Reference,
                    ParsePosition::Reference => Self::Freq,
                    ParsePosition::Freq => Self::Degree,
                    ParsePosition::Degree => Self::Keys,
                    ParsePosition::Keys => Self::Trailing,
                    ParsePosition::Trailing => panic!("this should not happen"),
                }
            }
        }
        let mut state = ParsePosition::MapSize;

        let mut res = KeyboardMapping::new();

        for (lineno, mut line) in kbm_contents.split('\n').map(|x| x.trim()).enumerate() {
            if line.starts_with('!') {
                continue;
            }

            if line == "x" {
                line = "-1";
            } else if !matches!(state, ParsePosition::Trailing) {
                let lc = line.chars();
                let mut valid_line = !line.is_empty();
                let mut bad_char = '\0';

                for val in lc {
                    if !valid_line || val == '\0' {
                        break;
                    }

                    if !(val == ' '
                        || val.is_ascii_digit()
                        || val == '.'
                        || val == 13 as char
                        || val == '\n')
                    {
                        valid_line = false;
                        bad_char = val;
                    }
                }

                if !valid_line {
                    return Err(TuningError::ParseError(format!(
                        "Bad character {bad_char} on line {lineno}"
                    )));
                }
            }

            let i = match line.parse::<i32>() {
                Err(_) => Err(TuningError::ParseError(format!(
                    "Value on line {lineno} is not integer value."
                ))),
                Ok(x) => Ok(x),
            };
            let v = match line.parse::<f64>() {
                Err(_) => Err(TuningError::ParseError(format!(
                    "Value on line {lineno} is not float value."
                ))),
                Ok(x) => Ok(x),
            };

            match state {
                ParsePosition::MapSize => res.count = i? as usize,
                ParsePosition::FirstMidi => res.first_midi = i?,
                ParsePosition::LastMidi => res.last_midi = i?,
                ParsePosition::Middle => res.middle_note = i?,
                ParsePosition::Reference => res.tuning_constant_note = i?,
                ParsePosition::Freq => {
                    res.tuning_frequency = v?;
                    res.tuning_pitch = res.tuning_frequency / MIDI_0_FREQ;
                }
                ParsePosition::Degree => res.octave_degrees = i?,
                ParsePosition::Keys => {
                    res.keys.push(i?);
                    if res.keys.len() == res.count {
                        state = ParsePosition::Trailing;
                    }
                }
                ParsePosition::Trailing => {}
            }

            if !(matches!(state, ParsePosition::Keys | ParsePosition::Trailing)) {
                state = state.next();
            }
            if matches!(state, ParsePosition::Keys) && res.count == 0 {
                state = ParsePosition::Trailing;
            }
        }

        if !matches!(state, ParsePosition::Keys | ParsePosition::Trailing) {
            return Err(TuningError::IncompleteKBM);
        }

        if res.keys.len() != res.count {
            return Err(TuningError::InvalidKeyCount);
        }

        res.raw_text = kbm_contents.to_string();
        Ok(res)
    }

    /// Creates a KeyboardMapping which keeps the MIDI note 60 (A4) set to a constant given
    /// frequency.
    pub fn tune_a69_to(freq: f64) -> Self {
        KeyboardMapping::tune_note_to(69, freq)
    }

    /// Creates a KeyboardMapping which keeps the MIDI note given set to a constant given
    /// frequency.
    pub fn tune_note_to(midi_note: i32, freq: f64) -> Self {
        KeyboardMapping::start_scale_on_and_tune_note_to(60, midi_note, freq)
    }

    /// Generates a KBM where `scale_start` is the note 0 of the scale, where `midi_note` is the
    /// tuned note, and where `freq` is the frequency.
    pub fn start_scale_on_and_tune_note_to(scale_start: i32, midi_note: i32, freq: f64) -> Self {
        let data = format!(
            "! Automatically generated mapping, tuning note {midi_note} to {freq} Hz
            !
            ! Size of map
            0
            ! First and last MIDI notes to map - map the entire keyboard
            0
            127
            ! Middle note where the first entry in the scale is mapped.
            {scale_start}
            ! Reference note where frequency is fixed
            {midi_note}
            ! Frequency for MIDI note {midi_note}
            {freq}
            ! Scale degree for formal octave. This is an empty mapping, so:
            0
            ! Mapping. This is an empty mapping so list no keys"
        );

        KeyboardMapping::parse_kbm_data(&data).expect("this should not fail")
    }
}

const N: usize = 512;

/// The Tuning struct is the primary place where you will interact with this library.
///
/// It is constrcted for a scale and mapping and then gives you the ability to determine frequencies
/// across and beyond the MIDI keyboard. Since modulation can force key number well outside the [0,
/// 127] range, we support a MIDI note range from -256 to +256 spanning more than the entire
/// frequency space reasonable.
///
/// To use this struct, you construct a fresh instance every time you want to use a different
/// [`Scale`] and [`KeyboardMapping`]. If you want to tune to a different scale or mapping, just
/// construct a new instance.
///
/// ```
/// # use tuning_library_rs::*;
/// let s = Scale::even_temperament_12_note_scale(); // or any other function constructing a Scale
/// let k = KeyboardMapping::tune_a69_to(432.0); // or any other function constructing a KeyboardMapping
///
/// let t1 = Tuning::from_scale(s.clone());
/// let t2 = Tuning::from_keyboard_mapping(k.clone());
/// let t3 = Tuning::from_scale_and_keyboard_mapping(s.clone(), k.clone(), AllowTuningOnUnmapped(false));
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug)]
pub struct Tuning {
    /// Scale of the tuning.
    pub scale: Scale,

    /// Keyboard mapping of the tuning.
    pub keyboard_mapping: KeyboardMapping,

    ptable: Vec<f64>,
    lptable: Vec<f64>,
    scale_position_table: Vec<i32>,
    allow_tuning_center_on_unmapped: bool,
}

impl Default for Tuning {
    fn default() -> Self {
        Self::new()
    }
}

impl Tuning {
    /// Constucts a `Tuning` with 12-EDO scale and standard mapping.
    pub fn new() -> Self {
        Tuning::from_scale_and_keyboard_mapping(
            Scale::even_temperament_12_note_scale(),
            KeyboardMapping::new(),
            AllowTuningOnUnmapped(false),
        )
        .unwrap()
    }

    /// Constructs a `Tuning` with given `scale` and standard mapping.
    pub fn from_scale(scale: Scale) -> Result<Self, TuningError> {
        Tuning::from_scale_and_keyboard_mapping(
            scale,
            KeyboardMapping::new(),
            AllowTuningOnUnmapped(false),
        )
    }

    /// Constructs a `Tuning` with 12-EDO scale and given `keyboard_mapping`.
    pub fn from_keyboard_mapping(keyboard_mapping: KeyboardMapping) -> Result<Self, TuningError> {
        Tuning::from_scale_and_keyboard_mapping(
            Scale::even_temperament_12_note_scale(),
            keyboard_mapping,
            AllowTuningOnUnmapped(false),
        )
    }

    /// Constructs a `Tuning` with given `scale` and `keyboard_mapping`.
    pub fn from_scale_and_keyboard_mapping(
        scale: Scale,
        keyboard_mapping: KeyboardMapping,
        allow_tuning_center_on_unmapped: AllowTuningOnUnmapped,
    ) -> Result<Self, TuningError> {
        let mut tun = Tuning {
            scale,
            keyboard_mapping,
            allow_tuning_center_on_unmapped: allow_tuning_center_on_unmapped.0,
            ptable: vec![0f64; N],
            lptable: vec![0f64; N],
            scale_position_table: vec![0; N],
        };

        let mut o_sp = 0;
        if tun.scale.count == 0 {
            return Err(TuningError::TooFewNotes);
        }

        let kbm_rotations = tun
            .keyboard_mapping
            .keys
            .iter()
            .map(|x| (*x as f64 / tun.scale.count as f64).ceil() as i32)
            .max()
            .unwrap_or(1);

        if kbm_rotations > 1 {
            let mut new_s = tun.scale.clone();
            new_s.count = tun.scale.count * kbm_rotations as usize;
            let back_cents = tun.scale.tones.last().unwrap().value.cents();
            let mut push_off = back_cents;

            for _ in 1..kbm_rotations {
                for t in &tun.scale.tones {
                    let mut t_copy = t.clone();
                    t_copy.value = ToneValue::Cents(t.value.cents() + push_off);
                    new_s.tones.push(t_copy);
                }
                push_off += back_cents;
            }

            tun.scale = new_s;
            tun.keyboard_mapping.octave_degrees *= kbm_rotations;
            if tun.keyboard_mapping.octave_degrees == 0 {
                tun.keyboard_mapping.octave_degrees = tun.scale.count as i32;
            }
        }

        if tun.keyboard_mapping.octave_degrees > tun.scale.count as i32 {
            return Err(TuningError::MappingLongerThanScale);
        }

        let mut pitches = [0.0; N];

        let pos_pitch_0 = 256 + tun.keyboard_mapping.tuning_constant_note;
        let pos_scale_0 = 256 + tun.keyboard_mapping.middle_note;

        let pitch_mod = tun.keyboard_mapping.tuning_pitch.log2() / 2f64.log2() - 1.0;

        let mut scale_position_of_tuning_note =
            tun.keyboard_mapping.tuning_constant_note - tun.keyboard_mapping.middle_note;

        if tun.keyboard_mapping.count > 0 {
            while scale_position_of_tuning_note >= tun.keyboard_mapping.count as i32 {
                scale_position_of_tuning_note -= tun.keyboard_mapping.count as i32;
            }

            while scale_position_of_tuning_note < 0 {
                scale_position_of_tuning_note += tun.keyboard_mapping.count as i32;
            }

            o_sp = scale_position_of_tuning_note;
            scale_position_of_tuning_note =
                tun.keyboard_mapping.keys[scale_position_of_tuning_note as usize];

            if scale_position_of_tuning_note == -1 && !tun.allow_tuning_center_on_unmapped {
                return Err(TuningError::TuningUnmappedKey);
            }
        }

        let tuning_center_pitch_offset;
        if scale_position_of_tuning_note == 0 {
            tuning_center_pitch_offset = 0.0;
        } else if scale_position_of_tuning_note == -1 && tun.allow_tuning_center_on_unmapped {
            let mut low = 0;
            let mut high = 0;
            let mut octave_up = false;
            let mut octave_down = false;

            let mut i = o_sp as usize - 1;
            while i != o_sp as usize {
                if tun.keyboard_mapping.keys[i] != -1 {
                    low = tun.keyboard_mapping.keys[i];
                    break;
                }

                if i > o_sp as usize {
                    octave_down = true;
                }

                i = (i - 1) % tun.keyboard_mapping.count;
            }

            i = o_sp as usize + 1;
            while i != o_sp as usize {
                if tun.keyboard_mapping.keys[i] != -1 {
                    high = tun.keyboard_mapping.keys[i];
                    break;
                }

                if i < o_sp as usize {
                    octave_up = true;
                }

                i = (i + 1) % tun.keyboard_mapping.count;
            }

            let dt = tun.scale.tones[tun.scale.count - 1].value.cents();
            let pitch_low = if octave_down {
                tun.scale.tones[low as usize - 1].value.cents() - dt
            } else {
                tun.scale.tones[low as usize - 1].value.float_value() - 1.0
            };
            let pitch_high = if octave_up {
                tun.scale.tones[high as usize - 1].value.cents() + dt
            } else {
                tun.scale.tones[high as usize - 1].value.float_value() - 1.0
            };
            tuning_center_pitch_offset = (pitch_high + pitch_low) / 2.0;
        } else {
            let mut tshift = 0.0;
            let dt = tun.scale.tones[tun.scale.count - 1].value.float_value() - 1.0;

            while scale_position_of_tuning_note < 0 {
                scale_position_of_tuning_note += tun.scale.count as i32;
                tshift += dt;
            }
            while scale_position_of_tuning_note > tun.scale.count as i32 {
                scale_position_of_tuning_note -= tun.scale.count as i32;
                tshift -= dt;
            }

            if scale_position_of_tuning_note == 0 {
                tuning_center_pitch_offset = -tshift;
            } else {
                tuning_center_pitch_offset = tun.scale.tones
                    [scale_position_of_tuning_note as usize - 1]
                    .value
                    .float_value()
                    - 1.0
                    - tshift;
            }
        }

        for (i, pitch) in pitches.iter_mut().enumerate().take(N) {
            let distance_from_pitch_0 = i as i32 - pos_pitch_0;
            let distance_from_scale_0 = i as i32 - pos_scale_0;

            if distance_from_pitch_0 == 0 {
                *pitch = 1.0;
                tun.lptable[i] = *pitch + pitch_mod;
                tun.ptable[i] = 2f64.powf(tun.lptable[i]);

                if tun.keyboard_mapping.count > 0 {
                    let mut mapping_key = distance_from_scale_0 % tun.keyboard_mapping.count as i32;
                    if mapping_key < 0 {
                        mapping_key += tun.keyboard_mapping.count as i32;
                    }

                    let cm = tun.keyboard_mapping.keys[mapping_key as usize];
                    if !tun.allow_tuning_center_on_unmapped && cm < 0 {
                        return Err(TuningError::TuningUnmappedKey);
                    }
                }

                tun.scale_position_table[i] =
                    scale_position_of_tuning_note % tun.scale.count as i32;
            } else {
                let mut rounds;
                let mut this_round;
                let mut disable = false;
                if tun.keyboard_mapping.count == 0 {
                    rounds = (distance_from_scale_0 - 1) / tun.scale.count as i32;
                    this_round = (distance_from_scale_0 - 1) % tun.scale.count as i32;
                } else {
                    let mut mapping_key = distance_from_scale_0 % tun.keyboard_mapping.count as i32;
                    if mapping_key < 0 {
                        mapping_key += tun.keyboard_mapping.count as i32;
                    }

                    let mut rotations = 0;
                    let mut dt = distance_from_scale_0;
                    if dt > 0 {
                        while dt >= tun.keyboard_mapping.count as i32 {
                            dt -= tun.keyboard_mapping.count as i32;
                            rotations += 1;
                        }
                    } else {
                        while dt < 0 {
                            dt += tun.keyboard_mapping.count as i32;
                            rotations -= 1;
                        }
                    }

                    let cm = tun.keyboard_mapping.keys[mapping_key as usize];

                    let mut push = 0;
                    if cm < 0 {
                        disable = true;
                    } else {
                        if cm > tun.scale.count as i32 {
                            return Err(TuningError::MappingLongerThanScale);
                        }
                        push = mapping_key - cm;
                    }

                    if tun.keyboard_mapping.octave_degrees > 0
                        && tun.keyboard_mapping.octave_degrees != tun.keyboard_mapping.count as i32
                    {
                        rounds = rotations;
                        this_round = cm - 1;
                        if this_round < 0 {
                            this_round = tun.keyboard_mapping.octave_degrees - 1;
                            rounds -= 1;
                        }
                    } else {
                        rounds = (distance_from_scale_0 - push - 1) / tun.scale.count as i32;
                        this_round = (distance_from_scale_0 - push - 1) % tun.scale.count as i32;
                    }
                }

                if this_round < 0 {
                    this_round += tun.scale.count as i32;
                    rounds -= 1;
                }

                if disable {
                    *pitch = 0.0;
                    tun.scale_position_table[i] = -1;
                } else {
                    *pitch = tun.scale.tones[this_round as usize].value.float_value()
                        + rounds as f64
                            * (tun.scale.tones[tun.scale.count - 1].value.float_value() - 1.0)
                        - tuning_center_pitch_offset;
                    tun.scale_position_table[i] = (this_round + 1) % tun.scale.count as i32;
                }

                tun.lptable[i] = *pitch + pitch_mod;
                tun.ptable[i] = 2f64.powf(*pitch + pitch_mod);
            }
        }

        Ok(tun)
    }

    /// Fills unmapped notes with interpolated vaules.
    pub fn with_skipped_notes_interpolated(&self) -> Self {
        let mut res = self.clone();

        for i in 1..N - 1 {
            if self.scale_position_table[i] >= 0 {
                continue;
            }

            let mut nxt = i + 1;
            let mut prv = i - 1;
            while self.scale_position_table[prv] < 0 {
                prv -= 1;
            }
            while nxt < N && self.scale_position_table[nxt] < 0 {
                nxt += 1;
            }
            let dist = (nxt - prv) as f64;
            let frac = (i - prv) as f64 / dist;
            res.lptable[i] = (1.0 - frac) * self.lptable[prv] + frac * self.lptable[nxt];
            res.ptable[i] = 2f64.powf(res.lptable[i]);
        }

        res
    }

    /// Retunrs the frequency in Hz for a given MIDI note.
    /// ```
    /// # use tuning_library_rs::*;
    /// let t = Tuning::new();
    /// assert!((t.frequency_for_midi_note(69) - 440.0).abs() < 1e-4); // A
    /// assert!((t.frequency_for_midi_note(60) - 261.6256).abs() < 1e-4); // middle C
    /// ```
    pub fn frequency_for_midi_note(&self, midi_note: i32) -> f64 {
        let mni = (N as i32 - 1).min(0.max(midi_note + 256));
        self.ptable[mni as usize] * MIDI_0_FREQ
    }

    /// Returns the frequency but with the standard frequency of MIDI note 0 divided out.
    /// ```
    /// # use tuning_library_rs::*;
    /// let t = Tuning::new();
    /// assert_eq!(t.frequency_for_midi_note_scaled_by_midi0(0), 1.0);
    /// assert_eq!(t.frequency_for_midi_note_scaled_by_midi0(60), 32.0);
    /// ```
    pub fn frequency_for_midi_note_scaled_by_midi0(&self, midi_note: i32) -> f64 {
        let mni = (N as i32 - 1).min(0.max(midi_note + 256));
        self.ptable[mni as usize]
    }

    /// Returns the log base 2 of the scaled frequency.
    /// ```
    /// # use tuning_library_rs::*;
    /// let t = Tuning::new();
    /// assert_eq!(t.log_scaled_frequency_for_midi_note(0), 0.0);
    /// assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);
    /// ```
    /// The value increases by one per frequency double.
    pub fn log_scaled_frequency_for_midi_note(&self, midi_note: i32) -> f64 {
        let mni = (N as i32 - 1).min(0.max(midi_note + 256));
        self.lptable[mni as usize]
    }

    /// Returns the space in the logical scale. Note 0 is the root. It has a maximum value of
    /// `count-1`. Note that SCL files omit the root internally and so this logical scale position
    /// is off by 1 from the index in the tones vector of the Scale data.
    pub fn scale_position_for_midi_note(&self, midi_note: i32) -> i32 {
        let mni = (N as i32 - 1).min(0.max(midi_note + 256));
        self.scale_position_table[mni as usize]
    }

    /// Returns whether a given `midi_note` is mapped in the `Tuning`'s `KeyboardMapping`.
    pub fn is_midi_note_mapped(&self, midi_note: i32) -> bool {
        let mni = (N as i32 - 1).min(0.max(midi_note + 256));
        self.scale_position_table[mni as usize] >= 0
    }
}
