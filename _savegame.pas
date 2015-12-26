unit _savegame;

interface

const num_saves = 9;
const num_high_scores = 5;
const save_name_length = 26;
const highscore_name_length = 26;

Type
  TSavFile = packed record
    unknown1: array[0..29] of byte;
    save_episode: array[0..num_saves-1] of word;
    save_levelnum: array[0..num_saves-1] of word;
    save_difficulty: array[0..num_saves-1] of word;
    save_name: array[0..num_saves-1, 0..save_name_length-1] of char;
    unknown2: array[0..9] of byte;
    save_score: array[0..num_saves-1] of cardinal;
    highscore_name: array[0..3, 0..num_high_scores-1, 0..highscore_name_length-1] of char;
    highscore_score: array[0..3, 0..num_high_scores-1] of cardinal;
    unknown3: array[0..25] of byte;
  end;

implementation

end.
