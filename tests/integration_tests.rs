use rand::{thread_rng, Rng};
use tuning_library_rs::*;

macro_rules! approx_eq {
    ($a:expr, $b:expr, $margin:expr) => {
        assert!(f64::abs($a - $b) < $margin, "Expected {}, got {}", $b, $a);
    };
}

#[test]
fn test_send() {
    fn assert_send<T: Send>() {}
    assert_send::<TuningError>();
    assert_send::<ToneValue>();
    assert_send::<Tone>();
    assert_send::<Scale>();
    assert_send::<KeyboardMapping>();
    assert_send::<Tuning>();
}

#[test]
fn test_sync() {
    fn assert_sync<T: Send>() {}
    assert_sync::<TuningError>();
    assert_sync::<ToneValue>();
    assert_sync::<Tone>();
    assert_sync::<Scale>();
    assert_sync::<KeyboardMapping>();
    assert_sync::<Tuning>();
}

fn test_file(filename: &str) -> String {
    format!("tests/data/{}", filename)
}

fn test_scls() -> Vec<String> {
    vec![
        "12-intune.scl",
        "12-shuffled.scl",
        "31edo.scl",
        "6-exact.scl",
        "marvel12.scl",
        "zeus22.scl",
        "ED4-17.scl",
        "ED3-17.scl",
        "31edo_dos_lineends.scl",
    ]
    .iter()
    .map(|x| x.to_string())
    .collect()
}

fn test_kbms() -> Vec<String> {
    vec![
        "empty-note61.kbm",
        "empty-note69.kbm",
        "mapping-a440-constant.kbm",
        "mapping-a442-7-to-12.kbm",
        "mapping-whitekeys-a440.kbm",
        "mapping-whitekeys-c261.kbm",
        "shuffle-a440-constant.kbm",
    ]
    .iter()
    .map(|x| x.to_string())
    .collect()
}

#[test]
fn load_a_12_tone_standard_tuning() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    assert_eq!(s.count(), 12);
}

#[test]
fn load_a_12_tone_standard_tuning_with_no_description() {
    let s = Scale::read_scl_file(test_file("12-intune-nodesc.scl")).unwrap();
    assert_eq!(s.count(), 12);
}

#[test]
fn kbm_file_from_text() {
    let data = "! A scale file
        ! with zero size
        0
        ! spanning the keybaord
        0
        127
        ! With C60 as constant and A as 452
        60
        69
        452
        ! and an octave might as well be zero
        0";
    KeyboardMapping::parse_kbm_data(data).unwrap();
}

#[test]
fn the_12_intune_tunes_properly() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    assert_eq!(s.count(), 12);

    let t = Tuning::from_scale(s).unwrap();
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 1e-10);
    assert_eq!(t.frequency_for_midi_note_scaled_by_midi0(60), 32.0);
    assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);
}

#[test]
fn the_12_intune_doubles_properly() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    for i in 0..12 {
        let mut note = -12 * 4 + i;
        let mut sc = t.frequency_for_midi_note_scaled_by_midi0(note);
        let mut lc = t.log_scaled_frequency_for_midi_note(note);
        while note < 200 {
            note += 12;
            let nlc = t.log_scaled_frequency_for_midi_note(note);
            let nsc = t.frequency_for_midi_note_scaled_by_midi0(note);
            approx_eq!(nsc, sc * 2.0, 1e-8);
            approx_eq!(nlc, lc + 1.0, 1e-8);
            sc = nsc;
            lc = nlc;
        }
    }
}

#[test]
fn scaling_is_constant() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    let f60 = t.frequency_for_midi_note(60);
    let fs60 = t.frequency_for_midi_note_scaled_by_midi0(60);

    for i in -200..200 {
        let f = t.frequency_for_midi_note(i);
        let fs = t.frequency_for_midi_note_scaled_by_midi0(i);
        assert_eq!(f / fs, f60 / fs60);
    }
}

#[test]
fn a440() {
    let k = KeyboardMapping::tune_a69_to(440.0);
    let t = Tuning::from_keyboard_mapping(k).unwrap();
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 1e-9);
    approx_eq!(t.frequency_for_midi_note(60), 261.625565301, 1e-9);
}

#[test]
fn a432() {
    let k = KeyboardMapping::tune_a69_to(432.0);
    let t = Tuning::from_keyboard_mapping(k).unwrap();
    approx_eq!(t.frequency_for_midi_note(69), 432.0, 1e-9);
    approx_eq!(
        t.frequency_for_midi_note(60),
        261.625565301 * 432.0 / 440.0,
        1e-9
    );
}

#[test]
fn random_as_scale_consistently() {
    let ut = Tuning::new();

    for _ in 0..100 {
        let fr = 400.0 + 80.0 * thread_rng().gen_range(0.0..=1.0);

        let k = KeyboardMapping::tune_a69_to(fr);
        let t = Tuning::from_keyboard_mapping(k).unwrap();
        approx_eq!(t.frequency_for_midi_note(69), fr, 1e-9);
        approx_eq!(
            t.frequency_for_midi_note(60),
            261.625565301 * fr / 440.0,
            1e-9
        );

        let ldiff =
            t.log_scaled_frequency_for_midi_note(69) - ut.log_scaled_frequency_for_midi_note(69);
        let ratio = t.frequency_for_midi_note(69) / ut.frequency_for_midi_note(69);

        for j in -200..200 {
            approx_eq!(
                t.log_scaled_frequency_for_midi_note(j) - ut.log_scaled_frequency_for_midi_note(j),
                ldiff,
                1e-8
            );
            approx_eq!(
                t.frequency_for_midi_note(69) / ut.frequency_for_midi_note(69),
                ratio,
                1e-8
            );
        }
    }
}

#[test]
fn test_all_constraints_scl_only() {
    for f in test_scls() {
        let s = Scale::read_scl_file(test_file(&f)).unwrap();
        let t = Tuning::from_scale(s).unwrap();

        for i in 0..127 {
            assert_eq!(
                t.frequency_for_midi_note(i),
                t.frequency_for_midi_note_scaled_by_midi0(i) * MIDI_0_FREQ
            );
            assert_eq!(
                t.frequency_for_midi_note_scaled_by_midi0(i),
                2f64.powf(t.log_scaled_frequency_for_midi_note(i))
            );
        }
    }
}

#[test]
fn test_all_constraints_kbm_only() {
    for f in test_kbms() {
        let k = KeyboardMapping::read_kbm_file(test_file(&f)).expect(&f);
        let t = Tuning::from_keyboard_mapping(k).unwrap();

        for i in 0..127 {
            assert_eq!(
                t.frequency_for_midi_note(i),
                t.frequency_for_midi_note_scaled_by_midi0(i) * MIDI_0_FREQ
            );
            assert_eq!(
                t.frequency_for_midi_note_scaled_by_midi0(i),
                2f64.powf(t.log_scaled_frequency_for_midi_note(i))
            );
        }
    }
}

#[test]
fn test_all_constraints_scl_and_kbm() {
    for fs in test_scls() {
        for fk in test_kbms() {
            let s = Scale::read_scl_file(test_file(&fs)).expect(&fs);
            let k = KeyboardMapping::read_kbm_file(test_file(&fk)).expect(&fk);

            if k.octave_degrees > s.count() as i32 {
                continue;
            }

            let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false))
                .unwrap();

            for i in 0..127 {
                assert_eq!(
                    t.frequency_for_midi_note(i),
                    t.frequency_for_midi_note_scaled_by_midi0(i) * MIDI_0_FREQ
                );
                assert_eq!(
                    t.frequency_for_midi_note_scaled_by_midi0(i),
                    2f64.powf(t.log_scaled_frequency_for_midi_note(i))
                );
            }
        }
    }
}

#[test]
fn non_monotonic_12_note() {
    let s = Scale::read_scl_file(test_file("12-shuffled.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    assert_eq!(t.scale.count(), 12);
    assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);

    let order = vec![0, 2, 1, 3, 5, 4, 6, 7, 8, 10, 9, 11, 12];
    let l60 = t.log_scaled_frequency_for_midi_note(60);

    for (i, ord) in order.iter().enumerate() {
        let li = t.log_scaled_frequency_for_midi_note(60 + i as i32);
        let oi = *ord as f64;
        approx_eq!(li - l60, oi / 12.0, 1e-6);
    }
}

#[test]
fn the_31_edo() {
    let s = Scale::read_scl_file(test_file("31edo.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    assert_eq!(t.scale.count(), 31);
    assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);

    let mut prev = t.log_scaled_frequency_for_midi_note(60);
    for i in 1..31 {
        let curr = t.log_scaled_frequency_for_midi_note(60 + i);
        approx_eq!(curr - prev, 1.0 / 31.0, 1e-6);
        prev = curr;
    }
}

#[test]
fn ed3_17() {
    let s = Scale::read_scl_file(test_file("ED3-17.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    assert_eq!(t.scale.count(), 17);
    assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);

    let mut prev = t.log_scaled_frequency_for_midi_note(60);
    for i in 1..40 {
        let curr = t.log_scaled_frequency_for_midi_note(60 + i);
        approx_eq!(2f64.powf(17.0 * (curr - prev)), 3.0, 1e-6);
        prev = curr;
    }
}

#[test]
fn ed4_17() {
    let s = Scale::read_scl_file(test_file("ED4-17.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    assert_eq!(t.scale.count(), 17);
    assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);

    let mut prev = t.log_scaled_frequency_for_midi_note(60);
    for i in 1..40 {
        let curr = t.log_scaled_frequency_for_midi_note(60 + i);
        approx_eq!(2f64.powf(17.0 * (curr - prev)), 4.0, 1e-6);
        prev = curr;
    }
}

#[test]
fn the_6_exact() {
    let s = Scale::read_scl_file(test_file("6-exact.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    assert_eq!(t.scale.count(), 6);
    assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);

    let known_values = vec![0.0, 0.22239, 0.41504, 0.58496, 0.73697, 0.87447, 1.0];

    for (i, known_value) in known_values.iter().enumerate() {
        approx_eq!(
            t.log_scaled_frequency_for_midi_note(60 + i as i32),
            t.log_scaled_frequency_for_midi_note(60) + known_value,
            1e-5
        );
    }
}

#[test]
fn carlos_alpha_one_step_scale() {
    let s = Scale::read_scl_file(test_file("carlos-alpha.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    assert_eq!(t.scale.count(), 1);
    assert_eq!(t.log_scaled_frequency_for_midi_note(60), 5.0);

    let diff = 2f64.powf(78.0 / 1200.0);
    for i in 30..80 {
        approx_eq!(
            t.frequency_for_midi_note_scaled_by_midi0(i) * diff,
            t.frequency_for_midi_note_scaled_by_midi0(i + 1),
            1e-5
        );
    }
}

#[test]
fn remapping_6_exact() {
    let s = Scale::read_scl_file(test_file("6-exact.scl")).unwrap();
    let t = Tuning::from_scale(s.clone()).unwrap();

    for _ in 0..100 {
        let mn = rand::random::<i32>() % 40 + 40;
        let freq = 150.0 + 300.0 * thread_rng().gen_range(0.0..=1.0);

        let k = KeyboardMapping::tune_note_to(mn, freq);
        let mapped =
            Tuning::from_scale_and_keyboard_mapping(s.clone(), k, AllowTuningOnUnmapped(false))
                .unwrap();

        approx_eq!(mapped.frequency_for_midi_note(mn), freq, 1e-6);

        // This scale is monotonic so test monotonicity still
        for i in 1..127 {
            if mapped.frequency_for_midi_note(i) > 1.0 {
                assert!(mapped.frequency_for_midi_note(i) > mapped.frequency_for_midi_note(i - 1));
            }
        }

        let n60ldiff = t.log_scaled_frequency_for_midi_note(60)
            - mapped.log_scaled_frequency_for_midi_note(60);
        for i in 0..128 {
            approx_eq!(
                t.log_scaled_frequency_for_midi_note(i)
                    - mapped.log_scaled_frequency_for_midi_note(i),
                n60ldiff,
                1e-6
            );
        }
    }
}

#[test]
fn remapping_31_edo() {
    let s = Scale::read_scl_file(test_file("31edo.scl")).unwrap();
    let t = Tuning::from_scale(s.clone()).unwrap();

    for _ in 0..100 {
        let mn = rand::random::<i32>() % 20 + 50;
        let freq = 150.0 + 300.0 * thread_rng().gen_range(0.0..=1.0);

        let k = KeyboardMapping::tune_note_to(mn, freq);
        let mapped =
            Tuning::from_scale_and_keyboard_mapping(s.clone(), k, AllowTuningOnUnmapped(false))
                .unwrap();

        approx_eq!(mapped.frequency_for_midi_note(mn), freq, 1e-6);

        // This scale is monotonic so test monotonicity still
        for i in 1..127 {
            if mapped.frequency_for_midi_note(i) > 1.0 {
                assert!(mapped.frequency_for_midi_note(i) > mapped.frequency_for_midi_note(i - 1));
            }
        }

        let n60ldiff = t.log_scaled_frequency_for_midi_note(60)
            - mapped.log_scaled_frequency_for_midi_note(60);
        for i in 0..128 {
            approx_eq!(
                t.log_scaled_frequency_for_midi_note(i)
                    - mapped.log_scaled_frequency_for_midi_note(i),
                n60ldiff,
                1e-6
            );
        }
    }
}

#[test]
fn remapping_ed4_17() {
    let s = Scale::read_scl_file(test_file("ED4-17.scl")).unwrap();
    let t = Tuning::from_scale(s.clone()).unwrap();

    for _ in 0..100 {
        let mn = rand::random::<i32>() % 40 + 40;
        let freq = 150.0 + 300.0 * thread_rng().gen_range(0.0..=1.0);

        let k = KeyboardMapping::tune_note_to(mn, freq);
        let mapped =
            Tuning::from_scale_and_keyboard_mapping(s.clone(), k, AllowTuningOnUnmapped(false))
                .unwrap();

        approx_eq!(mapped.frequency_for_midi_note(mn), freq, 1e-6);

        // This scale is monotonic so test monotonicity still
        for i in 1..127 {
            if mapped.frequency_for_midi_note(i) > 1.0 {
                assert!(mapped.frequency_for_midi_note(i) > mapped.frequency_for_midi_note(i - 1));
            }
        }

        let n60ldiff = t.log_scaled_frequency_for_midi_note(60)
            - mapped.log_scaled_frequency_for_midi_note(60);
        for i in 0..128 {
            approx_eq!(
                t.log_scaled_frequency_for_midi_note(i)
                    - mapped.log_scaled_frequency_for_midi_note(i),
                n60ldiff,
                1e-6
            );
        }
    }
}

#[test]
fn remapping_ed3_17() {
    let s = Scale::read_scl_file(test_file("ED3-17.scl")).unwrap();
    let t = Tuning::from_scale(s.clone()).unwrap();

    for _ in 0..100 {
        let mn = rand::random::<i32>() % 40 + 40;
        let freq = 150.0 + 300.0 * thread_rng().gen_range(0.0..=1.0);

        let k = KeyboardMapping::tune_note_to(mn, freq);
        let mapped =
            Tuning::from_scale_and_keyboard_mapping(s.clone(), k, AllowTuningOnUnmapped(false))
                .unwrap();

        approx_eq!(mapped.frequency_for_midi_note(mn), freq, 1e-6);

        // This scale is monotonic so test monotonicity still
        for i in 1..127 {
            if mapped.frequency_for_midi_note(i) > 1.0 {
                assert!(mapped.frequency_for_midi_note(i) > mapped.frequency_for_midi_note(i - 1));
            }
        }

        let n60ldiff = t.log_scaled_frequency_for_midi_note(60)
            - mapped.log_scaled_frequency_for_midi_note(60);
        for i in 0..128 {
            approx_eq!(
                t.log_scaled_frequency_for_midi_note(i)
                    - mapped.log_scaled_frequency_for_midi_note(i),
                n60ldiff,
                1e-6
            );
        }
    }
}

#[test]
fn kbm_12_intune_with_gap() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-c261.kbm")).unwrap();

    let t = Tuning::from_scale(s.clone()).unwrap();
    let tm = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    assert_eq!(t.scale.count(), 12);
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 1e-6);

    let maps = vec![
        (60, 60),
        (61, 62),
        (62, 64),
        (63, 65),
        (64, 67),
        (65, 69),
        (66, 71),
    ];

    for p in maps {
        approx_eq!(
            t.log_scaled_frequency_for_midi_note(p.0),
            tm.log_scaled_frequency_for_midi_note(p.1),
            1e-5
        );
    }
}

#[test]
fn piano_kbm() {
    let k = KeyboardMapping::read_kbm_file(test_file("piano.kbm")).unwrap();
    assert_eq!(k.count(), 0);
}

#[test]
fn the_128_kbm() {
    let k = KeyboardMapping::read_kbm_file(test_file("128.kbm")).unwrap();
    assert_eq!(k.count(), 0);
}

#[test]
fn non_monotonic_kbm_note() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("shuffle-a440-constant.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    assert_eq!(t.scale.count(), 12);
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 1e-6);

    let order = vec![0, 2, 1, 3, 4, 6, 5, 7, 8, 9, 11, 10, 12];
    let l60 = t.log_scaled_frequency_for_midi_note(60);

    for (i, oi) in order.iter().enumerate() {
        let li = t.log_scaled_frequency_for_midi_note(60 + i as i32);
        approx_eq!(li - l60, *oi as f64 / 12.0, 1e-6);
    }
}

#[test]
fn read_non_present_files() {
    let s = Scale::read_scl_file("blahlfdsfds");
    let k = KeyboardMapping::read_kbm_file("blahlfdsfds");

    assert!(matches!(s, Err(TuningError::FileError)));
    assert!(matches!(k, Err(TuningError::FileError)));
}

#[test]
fn mappings_bigger_than_scales_err() {
    let mut tested_at_least_one = false;

    for fs in test_scls() {
        for fk in test_kbms() {
            let s = Scale::read_scl_file(test_file(&fs)).unwrap();
            let k = KeyboardMapping::read_kbm_file(test_file(&fk)).unwrap();

            if k.octave_degrees <= s.count() as i32 {
                continue;
            }

            tested_at_least_one = true;

            assert!(matches!(
                Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)),
                Err(TuningError::MappingLongerThanScale)
            ));
        }
    }

    assert!(tested_at_least_one);
}

#[test]
fn bad_scl() {
    // Trailing data is OK
    Scale::read_scl_file(test_file("bad/extraline.scl")).unwrap();

    let s = Scale::read_scl_file(test_file("bad/badnote.scl"));
    assert!(matches!(s, Err(TuningError::ParseError(_))));

    let s = Scale::read_scl_file(test_file("bad/blanknote.scl"));
    assert!(matches!(s, Err(TuningError::InvalidNoteCount)));

    let s = Scale::read_scl_file(test_file("bad/badnote.scl"));
    assert!(matches!(s, Err(TuningError::ParseError(_))));
}

#[test]
fn bad_kbm() {
    let k = KeyboardMapping::read_kbm_file(test_file("bad/blank-line.kbm"));
    assert!(matches!(k, Err(TuningError::ParseError(_))));

    let k = KeyboardMapping::read_kbm_file(test_file("bad/empty-bad.kbm"));
    assert!(matches!(k, Err(TuningError::ParseError(_))));

    let k = KeyboardMapping::read_kbm_file(test_file("bad/garbage-key.kbm"));
    assert!(matches!(k, Err(TuningError::ParseError(_))));

    KeyboardMapping::read_kbm_file(test_file("bad/empty-extra.kbm")).unwrap();
    KeyboardMapping::read_kbm_file(test_file("bad/extraline-long.kbm")).unwrap();

    let k = KeyboardMapping::read_kbm_file(test_file("bad/missing-note.kbm"));
    assert!(matches!(k, Err(TuningError::ParseError(_))));
}

#[test]
fn bad_scl_data() {
    let s = Scale::parse_scl_data("");
    assert!(matches!(s, Err(TuningError::IncompleteSCL)));
}

#[test]
fn bad_kbm_data() {
    let k = KeyboardMapping::parse_kbm_data("");
    assert!(matches!(k, Err(TuningError::ParseError(_))));
}

#[test]
fn generate_ed2() {
    let s = Scale::even_division_of_span_by_m(2, 12).unwrap();
    assert_eq!(s.count(), 12);
    assert!(s.raw_text.len() > 1);

    let ut = Tuning::new();
    let t = Tuning::from_scale(s).unwrap();

    for i in 0..128 {
        assert_eq!(
            t.log_scaled_frequency_for_midi_note(i),
            ut.log_scaled_frequency_for_midi_note(i)
        );
    }
}

#[test]
fn generate_ed3_17() {
    let s = Scale::even_division_of_span_by_m(3, 17).unwrap();
    let sf = Scale::read_scl_file(test_file("ED3-17.scl")).unwrap();

    let ut = Tuning::from_scale(sf).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    for i in 0..128 {
        approx_eq!(
            t.log_scaled_frequency_for_midi_note(i),
            ut.log_scaled_frequency_for_midi_note(i),
            1e-6
        );
    }
}

#[test]
fn generate_ed4_17() {
    let s = Scale::even_division_of_span_by_m(4, 17).unwrap();
    let sf = Scale::read_scl_file(test_file("ED4-17.scl")).unwrap();

    let ut = Tuning::from_scale(sf).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    for i in 0..128 {
        approx_eq!(
            t.log_scaled_frequency_for_midi_note(i),
            ut.log_scaled_frequency_for_midi_note(i),
            1e-6
        );
    }
}

#[test]
fn constraints_on_random_ednm() {
    for _ in 0..100 {
        let span = rand::random::<u32>() % 7 + 2;
        let m = rand::random::<u32>() % 50 + 3;

        let s = Scale::even_division_of_span_by_m(span, m).unwrap();

        assert_eq!(s.count(), m as usize);
        assert!(s.raw_text.len() > 1);

        let t = Tuning::from_scale(s).unwrap();
        approx_eq!(
            t.frequency_for_midi_note_scaled_by_midi0(60) * span as f64,
            t.frequency_for_midi_note_scaled_by_midi0(60 + m as i32),
            1e-7
        );

        let d0 = t.log_scaled_frequency_for_midi_note(1) - t.log_scaled_frequency_for_midi_note(0);
        for i in 1..128 {
            approx_eq!(
                t.log_scaled_frequency_for_midi_note(i)
                    - t.log_scaled_frequency_for_midi_note(i - 1),
                d0,
                1e-7
            );
        }
    }
}

#[test]
fn edmn_errors() {
    assert!(matches!(
        Scale::even_division_of_span_by_m(0, 12),
        Err(TuningError::ZeroSpan)
    ));
    assert!(matches!(
        Scale::even_division_of_span_by_m(2, 0),
        Err(TuningError::ZeroSteps)
    ));
    assert!(matches!(
        Scale::even_division_of_span_by_m(0, 0),
        Err(TuningError::ZeroSpan) | Err(TuningError::ZeroSteps)
    ));
}

#[test]
fn kbm_generator() {
    for _ in 0..100 {
        let n = rand::random::<u32>() % 60 + 30;
        let fr = 1000.0 * thread_rng().gen_range(0.0..=1.0) + 50.0;
        let k = KeyboardMapping::tune_note_to(n as i32, fr);

        assert_eq!(k.tuning_constant_note, n as i32);
        assert_eq!(k.tuning_frequency, fr);
        assert_eq!(k.tuning_pitch, k.tuning_frequency / MIDI_0_FREQ);
        assert!(k.raw_text.len() > 1);
    }
}

#[test]
fn dos_scl() {
    Scale::read_scl_file(test_file("12-intune-dosle.scl")).unwrap();
}

#[test]
fn properly_read_a_file_with_dos_line_endings() {
    let s = Scale::read_scl_file(test_file("31edo_dos_lineends.scl")).unwrap();
    assert_eq!(s.count(), 31);
    assert_eq!(s.description, "31 equal divisions of octave");

    // the parsing should have the same floatvalues independent of crlf status obviously
    let q = Scale::read_scl_file(test_file("31edo.scl")).unwrap();
    for i in 0..q.count() {
        assert_eq!(q.tones[i].float_value(), s.tones[i].float_value())
    }
}

#[test]
fn dos_kbm() {
    let k = KeyboardMapping::read_kbm_file(test_file("empty-note69-dosle.kbm")).unwrap();
    assert_eq!(k.tuning_constant_note, 69);
}

#[test]
fn blank_scl() {
    let s = Scale::parse_scl_data("");
    assert!(matches!(s, Err(TuningError::IncompleteSCL)));

    // but what if we do construct a bad one?
    let mut s = Scale::new();
    s.tones.clear();
    let t = Tuning::from_scale(s);
    assert!(matches!(t, Err(TuningError::TooFewNotes)));
}

#[test]
fn valid_tones() {
    let t1 = Tone::from_string("130.0", None).unwrap();
    assert!(matches!(t1.value, ToneValue::Cents(_)));
    assert_eq!(t1.cents(), 130.0);
    assert_eq!(t1.float_value(), 130.0 / 1200.0 + 1.0);

    let t2 = Tone::from_string("7/2", None).unwrap();
    assert_eq!(t2.value, ToneValue::Ratio(7, 2));
    approx_eq!(t2.float_value(), (7_f64 / 2_f64).log2() + 1.0, 1e-6);

    let t3 = Tone::from_string("3", None).unwrap();
    assert_eq!(t3.value, ToneValue::Ratio(3, 1));
    approx_eq!(t3.float_value(), (3_f64 / 1_f64).log2() + 1.0, 1e-6);
}

#[test]
fn ridiculously_long_fraction_tones() {
    let mut top = 3;
    let mut bottom = 2;

    for _ in 0..18 {
        let frac = format!("{top}/{bottom}");
        let t = Tone::from_string(&frac, None).unwrap();
        assert_eq!(t.value, ToneValue::Ratio(top, bottom));
        top *= 10;
        bottom *= 10;
    }
}

#[test]
fn error_tones() {
    assert!(matches!(
        Tone::from_string("Not a number", None),
        Err(TuningError::ParseError(_))
    ));

    assert!(matches!(
        Tone::from_string("100.200 with extra stuff", None),
        Err(TuningError::ParseError(_))
    ));

    assert!(matches!(
        Tone::from_string("7/4/2", None),
        Err(TuningError::ParseError(_))
    ));

    assert!(matches!(
        Tone::from_string("7*2", None),
        Err(TuningError::ParseError(_))
    ));
}

#[test]
fn scale_position_untuned() {
    let t = Tuning::new();
    for i in 0..127 {
        assert_eq!(t.scale_position_for_midi_note(i), i % 12);
    }
}

#[test]
fn scale_position_untuned_mapped() {
    let k = KeyboardMapping::start_scale_on_and_tune_note_to(60, 69, 440.0);
    let t = Tuning::from_keyboard_mapping(k).unwrap();

    for i in 0..127 {
        assert_eq!(t.scale_position_for_midi_note(i), i % 12);
    }

    for _ in 0..100 {
        let n = rand::random::<u32>() % 60 + 30;
        let k = KeyboardMapping::start_scale_on_and_tune_note_to(n as i32, 69, 440.0);
        let t = Tuning::from_keyboard_mapping(k).unwrap();

        for i in 0..127 {
            assert_eq!(
                t.scale_position_for_midi_note(i),
                (i + 12 - n as i32 % 12) % 12
            );
        }
    }

    let k = KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-c261.kbm")).unwrap();
    let t = Tuning::from_keyboard_mapping(k).unwrap();

    let maps = vec![(0, 0), (2, 1), (4, 2), (5, 3), (7, 4), (9, 5), (11, 6)];

    for i in 0..127 {
        let spn = t.scale_position_for_midi_note(i);
        let mut expected = -1;
        for (first, second) in &maps {
            if i % 12 == *first {
                expected = *second;
            }
        }
        assert_eq!(spn, expected);
    }
}

#[test]
fn scale_position_tuned_unmapped() {
    let s = Scale::read_scl_file(test_file("zeus22.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    let mut off = 60;
    while off > 0 {
        off -= t.scale.count() as i32;
    }

    for i in 0..127 {
        assert_eq!(
            t.scale_position_for_midi_note(i),
            (i - off) % t.scale.count() as i32
        );
    }

    let s = Scale::read_scl_file(test_file("6-exact.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    let mut off = 60;
    while off > 0 {
        off -= t.scale.count() as i32;
    }

    for i in 0..127 {
        assert_eq!(
            t.scale_position_for_midi_note(i),
            (i - off) % t.scale.count() as i32
        );
    }
}

#[test]
fn scale_position_tuned_mapped() {
    for _ in 0..100 {
        let n = rand::random::<u32>() % 60 + 30;

        let s = Scale::read_scl_file(test_file("zeus22.scl")).unwrap();
        let k = KeyboardMapping::start_scale_on_and_tune_note_to(n as i32, 69, 440.0);
        let t =
            Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

        let mut off = n as i32;
        while off > 0 {
            off -= t.scale.count() as i32;
        }

        for i in 0..127 {
            assert_eq!(
                t.scale_position_for_midi_note(i),
                (i - off) % t.scale.count() as i32
            );
        }
    }
}

#[test]
fn all_scales_with_default_kbm() {
    for scl in test_scls() {
        let s = Scale::read_scl_file(test_file(&scl)).unwrap();
        let t = Tuning::from_scale(s).unwrap();
        assert_eq!(t.frequency_for_midi_note_scaled_by_midi0(60), 32.0);
    }
}

#[test]
fn the_31edo_with_mean_tone_mapping() {
    // Even though we have a 31 note octave we have 12 key mapping
    let s = Scale::read_scl_file(test_file("31edo.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("31edo_meantone.kbm")).unwrap();

    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 1e-9);
    approx_eq!(t.frequency_for_midi_note(69 + 12), 880.0, 1e-9);
}

#[test]
fn perfect_5th_unmapped() {
    let s = Scale::read_scl_file(test_file("12-ET-P5.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();

    for i in (60 - 36..127).step_by(12) {
        let f = t.frequency_for_midi_note(i);
        let f5 = t.frequency_for_midi_note(i + 7);
        approx_eq!(f5, f * 1.5, 1e-6);
    }
}

#[test]
fn perfect_5th_07_mapping() {
    let s = Scale::read_scl_file(test_file("12-ET-P5.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("mapping-n60-fifths.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    for i in (60..70).step_by(2) {
        let f = t.frequency_for_midi_note(i);
        let f5 = t.frequency_for_midi_note(i + 1);
        approx_eq!(f5, f * 1.5, 1e-6);
    }
}

#[test]
fn kbm_constructor_rawtext() {
    let k = KeyboardMapping::new();
    let kparse = KeyboardMapping::parse_kbm_data(&k.raw_text).unwrap();

    assert_eq!(k.count(), kparse.count());
    assert_eq!(k.first_midi, kparse.first_midi);
    assert_eq!(k.last_midi, kparse.last_midi);
    assert_eq!(k.middle_note, kparse.middle_note);
    assert_eq!(k.tuning_constant_note, kparse.tuning_constant_note);
    approx_eq!(k.tuning_frequency, kparse.tuning_frequency, 1e-9);
    assert_eq!(k.octave_degrees, kparse.octave_degrees);
}

#[test]
fn default_tuning_skips_nothing() {
    let t = Tuning::new();
    for i in 0..128 {
        assert!(t.is_midi_note_mapped(i));
    }
}

#[test]
fn scl_only_tuning_skips_nothing() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let t = Tuning::from_scale(s).unwrap();
    for i in 0..128 {
        assert!(t.is_midi_note_mapped(i));
    }
}

#[test]
fn kbm_only_tuning_absent_skips_skips_nothing() {
    let k = KeyboardMapping::read_kbm_file(test_file("empty-note69.kbm")).unwrap();
    let t = Tuning::from_keyboard_mapping(k).unwrap();
    for i in 0..128 {
        assert!(t.is_midi_note_mapped(i));
    }
}

#[test]
fn fully_mapped() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("empty-note69.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();
    for i in 0..128 {
        assert!(t.is_midi_note_mapped(i));
    }
}

#[test]
fn gaps_in_the_maps() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    for k in 0..128 {
        let i = k % 12;
        let is_on = i == 0 || i == 2 || i == 4 || i == 5 || i == 7 || i == 9 || i == 11;
        assert_eq!(t.is_midi_note_mapped(k), is_on);
    }
}

#[test]
fn gaps_in_the_maps_kbm_only() {
    let k = KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-a440.kbm")).unwrap();
    let t = Tuning::from_keyboard_mapping(k).unwrap();

    for k in 0..128 {
        let i = k % 12;
        let is_on = i == 0 || i == 2 || i == 4 || i == 5 || i == 7 || i == 9 || i == 11;
        assert_eq!(t.is_midi_note_mapped(k), is_on);
    }
}

#[test]
fn tuning_with_gaps() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    for k in 2..128 {
        let i = k % 12;
        let is_on = i == 0 || i == 2 || i == 4 || i == 5 || i == 7 || i == 9 || i == 11;
        let prior_on = if i == 0 || i == 5 { 1 } else { 2 };

        if is_on {
            assert!(
                t.log_scaled_frequency_for_midi_note(k)
                    > t.log_scaled_frequency_for_midi_note(k - prior_on)
            );
        }
    }
}

#[test]
fn tuning_with_gaps_and_interpolation() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false))
        .unwrap()
        .with_skipped_notes_interpolated();

    for k in 2..128 {
        let i = k % 12;
        let is_on = i == 0 || i == 2 || i == 4 || i == 5 || i == 7 || i == 9 || i == 11;

        assert_eq!(t.is_midi_note_mapped(k), is_on);

        assert!(
            t.log_scaled_frequency_for_midi_note(k) > t.log_scaled_frequency_for_midi_note(k - 1)
        );
        assert!(t.frequency_for_midi_note(k) > t.frequency_for_midi_note(k - 1));
        assert!(
            t.frequency_for_midi_note_scaled_by_midi0(k)
                > t.frequency_for_midi_note_scaled_by_midi0(k - 1)
        );
    }
}

#[test]
fn tuning_from_60_works() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    assert!(t.is_midi_note_mapped(60));
    assert!(t.is_midi_note_mapped(69));
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 1e-9);
}

#[test]
fn tuning_from_59_throws() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k =
        KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-from-59-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false));

    assert!(matches!(t, Err(TuningError::TuningUnmappedKey)));
}

#[test]
fn tuning_from_59_no_throw() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k =
        KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-from-59-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(true)).unwrap();

    approx_eq!(t.frequency_for_midi_note(59), 246.94, 0.01);
}

#[test]
fn tuning_from_59_altmapping() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k =
        KeyboardMapping::read_kbm_file(test_file("mapping-whitekeysalt-from-59-a440.kbm")).unwrap();

    let t =
        Tuning::from_scale_and_keyboard_mapping(s.clone(), k.clone(), AllowTuningOnUnmapped(false));
    assert!(matches!(t, Err(TuningError::TuningUnmappedKey)));

    let t =
        Tuning::from_scale_and_keyboard_mapping(s.clone(), k.clone(), AllowTuningOnUnmapped(false));
    assert!(matches!(t, Err(TuningError::TuningUnmappedKey)));

    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(true)).unwrap();
    approx_eq!(
        t.frequency_for_midi_note(59),
        440.0 * 2f64.powf(-(5.5 / 12.0)),
        0.01
    );
}

#[test]
fn tuning_from_48_works() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k =
        KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-from-48-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    assert!(t.is_midi_note_mapped(60));
    assert!(t.is_midi_note_mapped(69));
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 0.01);
}

#[test]
fn tuning_from_48_works_with_interpolation() {
    let s = Scale::read_scl_file(test_file("12-intune.scl")).unwrap();
    let k =
        KeyboardMapping::read_kbm_file(test_file("mapping-whitekeys-from-48-a440.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false))
        .unwrap()
        .with_skipped_notes_interpolated();

    assert!(t.is_midi_note_mapped(60));
    assert!(t.is_midi_note_mapped(69));
    approx_eq!(t.frequency_for_midi_note(69), 440.0, 0.01);
}

#[test]
fn wrapped_kbms() {
    for kbm in [
        "empty-c-100.kbm",
        "fournote-c-100.kbm",
        "eightnote-c-100.kbm",
        "twelve-c-100.kbm",
    ] {
        let s = Scale::read_scl_file(test_file("kbm-fix-012023/exact4.scl")).unwrap();
        let k =
            KeyboardMapping::read_kbm_file(test_file(&format!("kbm-fix-012023/{kbm}"))).unwrap();
        let t =
            Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

        assert_eq!(t.frequency_for_midi_note(60), 100.0);

        for i in 61..76 {
            let ni = t.frequency_for_midi_note(i);
            let np = t.frequency_for_midi_note(i - 1);
            let diff = match i {
                (0..=64) => 25,
                (65..=68) => 50,
                (69..=72) => 100,
                _ => 200,
            };

            assert!(ni > np);
            approx_eq!(ni - np, diff as f64, 1e-9);
        }
    }
}

#[test]
fn skipper_one() {
    let s = Scale::read_scl_file(test_file("kbm-fix-012023/exact4.scl")).unwrap();
    let k =
        KeyboardMapping::read_kbm_file(test_file("kbm-fix-012023/threenote-c-100.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    for (v, note) in vec![100.0, 125.0, 175.0, 200.0, 250.0, 350.0, 400.0]
        .iter()
        .zip(60..)
    {
        approx_eq!(t.frequency_for_midi_note(note), v, 1e-9);
    }
}

#[test]
fn skipper_59() {
    let s = Scale::read_scl_file(test_file("kbm-fix-012023/exact4.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("kbm-fix-012023/threenote-c-100-from-1.kbm"))
        .unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    for (v, note) in vec![100.0, 140.0, 160.0, 200.0, 280.0, 320.0]
        .iter()
        .zip(60..)
    {
        approx_eq!(t.frequency_for_midi_note(note), v, 1e-9);
    }
}

#[test]
fn skipper_long() {
    let s = Scale::read_scl_file(test_file("kbm-fix-012023/exact4.scl")).unwrap();
    let k = KeyboardMapping::read_kbm_file(test_file("kbm-fix-012023/sixnote-c-100.kbm")).unwrap();
    let t = Tuning::from_scale_and_keyboard_mapping(s, k, AllowTuningOnUnmapped(false)).unwrap();

    for (v, note) in vec![100.0, 125.0, 175.0, 200.0, 250.0, 350.0, 400.0]
        .iter()
        .zip(60..)
    {
        approx_eq!(t.frequency_for_midi_note(note), v, 1e-9);
    }
}
