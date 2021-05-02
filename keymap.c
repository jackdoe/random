/*
 * Copyright (c) 2018 Charlie Waters
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "vortex.h"

const uint16_t keymaps_default[][MATRIX_ROWS][MATRIX_COLS] =
  {
#if defined(KEYMAP_60_ANSI)
   [0] = LAYOUT_60_ansi(
			KC_GRV,  KC_1,    KC_2,    KC_3,    KC_4,       KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS, KC_EQL,  KC_BSPC,
			KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,       KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC, KC_RBRC, KC_BSLS,
			KC_LCTL, KC_A,    KC_S,    KC_D,    KC_F,       KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT, KC_ENT,
			KC_LSFT, KC_Z,    KC_X,    KC_C,    KC_V,       KC_B,    KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RSFT,
			MO(2),   KC_LGUI, KC_LALT, KC_SPC,  MO(2),      TG(3),   KC_RGUI, MO(2)
			),

   [1] = LAYOUT_60_ansi(
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______			
			),

   
   [2] = LAYOUT_60_ansi(
			KC_ESC,  KC_F1,     KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,    KC_F9,    KC_F10,   KC_F11,   KC_F12,  KC_DEL,
			_______, _______,   _______, _______, _______, _______, _______, _______, KC_PAUS,  KC_SLCK,  KC_PSCR,  KC_UP,    _______, _______,
			_______, KC_VOLD,   KC_VOLU, KC_MUTE, _______, _______, _______, _______, KC_HOME,  KC_PGUP,  KC_LEFT,  KC_RIGHT, _______,
			_______, _______,   _______, _______, _______, _______, _______, _______, KC_END,   KC_PGDN,  KC_DOWN,  _______,
			_______, _______,   _______, _______, _______, _______, _______, _______
			),
   [3] = LAYOUT_60_ansi(
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
			_______, _______,  _______, _______, _______, _______, _______, _______			
			),

#else
#error "No Keymap!"
#endif
  };

const uint16_t keymaps_default_size = sizeof(keymaps_default);
uint16_t keymaps[MAX_LAYERS][MATRIX_ROWS][MATRIX_COLS];

const uint16_t PROGMEM fn_actions[] = {
};



void matrix_init_user(void) {

}

void matrix_scan_user(void) {

}
static uint8_t saved_mod;
static uint8_t saved_code;

bool swap(uint8_t mod, uint8_t from, uint8_t to, bool pressed) {
  if (pressed) {
    if (get_mods() == mod) {
      del_mods(mod);
      send_keyboard_report();
      saved_mod = mod;
      saved_code = to;
    } else {
      saved_mod = 0;
      saved_code = from;
    }
    register_code(saved_code);
    send_keyboard_report();
  } else {
    unregister_code(saved_code);
    send_keyboard_report();
    if (saved_mod) {
      add_mods(saved_mod);
      send_keyboard_report();
    }

    saved_code = 0;
    saved_mod = 0;
  }
  return false;
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  uint8_t layer = biton32(layer_state);
  if (layer == 3) {
    switch(keycode) {
    case KC_G: return swap(MOD_LCTL, KC_G, KC_ESC,  record->event.pressed); break;
    case KC_J: return swap(MOD_LCTL, KC_J, KC_ENT,  record->event.pressed); break;
    case KC_H: return swap(MOD_LCTL, KC_H, KC_BSPC, record->event.pressed); break;
    case KC_P: return swap(MOD_LCTL, KC_P, KC_UP,   record->event.pressed); break;
    case KC_N: return swap(MOD_LCTL, KC_N, KC_DOWN, record->event.pressed); break;
    case KC_A: return swap(MOD_LCTL, KC_A, KC_HOME, record->event.pressed); break;
    case KC_D: return swap(MOD_LCTL, KC_D, KC_DEL,  record->event.pressed); break;
    case KC_E: return swap(MOD_LCTL, KC_E, KC_END,  record->event.pressed); break;
    case KC_V: {
      uint8_t m = get_mods();
      if (m == MOD_LALT || saved_mod == MOD_LALT) {
	return swap(MOD_LALT, KC_V, KC_PGUP,  record->event.pressed);
      } else if (m == MOD_LCTL || saved_mod == MOD_LCTL) {
	return swap(MOD_LCTL, KC_V, KC_PGDN,  record->event.pressed);
      }
      break;
    }
    }
  } else {
    switch(keycode) {
    case KC_G: return swap(MOD_LCTL, KC_G, KC_ESC,  record->event.pressed); break;
    case KC_J: return swap(MOD_LCTL, KC_J, KC_ENT,  record->event.pressed); break;
    case KC_H: return swap(MOD_LCTL, KC_H, KC_BSPC, record->event.pressed); break;
    }
  }
  return true;
}

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt) {
  /*
  switch(id) {
  case 1:
    if (record->event.pressed) {
      SEND_STRING("if err := ; err != nil {");
      return MACRO(T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),T(LEFT),END);
    }
    break;
  case 2:
    if (record->event.pressed) {
      SEND_STRING("kubectl get pods -n live | grep ");
      return MACRO(END);
    }
    break;
  }
  */
  return MACRO_NONE;
};



void led_set_user(uint8_t usb_led) {
}
