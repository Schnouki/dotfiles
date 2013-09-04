%module lousy
%include <stdint.i>

void init();
uint32_t idle();
void change_brightness(double delta);
uint8_t get_caps_lock();
uint8_t get_num_lock();
