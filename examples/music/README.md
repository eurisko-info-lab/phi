# 🎹 Phi Music: Building the Piano and the Player

> *"A man's job is to teach his kids to build the piano and the piano's player."*

This module doesn't just play music—it teaches Phi to **understand** music from first principles.

## Philosophy

Most programs play pre-recorded sounds. That's giving a fish.

We teach Phi to:
1. **Build the piano** - From the physics of vibrating strings to the mathematics of harmony
2. **Build the player** - From musical notation to interpretation and rendering

## The Files

### [piano.phi](piano.phi) - The Instrument

Defines a piano from first principles:

```
Frequency → Harmonics → Envelope → Tone
     ↓          ↓           ↓        ↓
   440 Hz    f,2f,3f...   ADSR    Sample
```

- **Pitch Mathematics**: Equal temperament, semitones, octaves
- **Harmonic Series**: Why a piano sounds like a piano
- **ADSR Envelope**: Attack, decay, sustain, release
- **88 Keys**: The full keyboard

### [player.phi](player.phi) - The Musician

Defines how to read and perform music:

```
Score → Events → Interpretation → Samples
  ↓        ↓           ↓             ↓
Notes   PlayNote    Timing       Audio
Rests   Tempo       Dynamics     WAV
Chords  Dynamic     Mix
```

- **Musical Notation**: Note values, rests, chords
- **Dynamics**: pp, p, mp, mf, f, ff
- **Articulation**: Legato, staccato, accent
- **Interpretation**: Score → Sound

## Example: Eine kleine Nachtmusik

```phi
{ beat = 0.0, event = PlayNote (G 4) Eighth f },
{ beat = 0.5, event = Rest Eighth },
{ beat = 1.0, event = PlayNote (G 4) Eighth f },
...
```

The famous opening: *G-G-G, D... G-G-G, B...*

## The Lesson

```
┌─────────────────────────────────────────────────────────┐
│                                                         │
│   Give someone a WAV file → They hear music once        │
│                                                         │
│   Teach them to build the piano and player →            │
│   They can create any music, forever                    │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

This is the Phi way: **meta over object**, **understanding over usage**.

## Future Work

- [ ] Other instruments (violin, flute, drums)
- [ ] MIDI import/export
- [ ] Real-time synthesis
- [ ] Compile to Faust DSP
- [ ] Neural interpretation (expression, rubato)

---

*"The purpose of music is to help us understand ourselves." — Mozart*
