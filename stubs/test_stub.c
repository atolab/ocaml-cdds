#include "dds_stubs.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

void run_pub() {
  dds_entity_t dp = s_create_participant(0);
  dds_entity_t p = s_create_pub(dp);
  // dds_entity_t p = s_create_pub_wp(dp, "alpha");
  dds_entity_t t = s_create_topic_sksv(dp, "KeyValue");
  dds_entity_t w = s_create_state_writer(p, t);

  char value[256];
  int i = 0;
  while (true) {
    printf("Writing...\n");
    sprintf(value, "value-%d", i);
    i++;
    s_write_key_value(w, "key", value);
    sleep(1);
  }
}

void listener(dds_entity_t rd, void* attch) {
  printf("Listener called!\n");
  dds_bit_SKeySValue* sample = s_take_sksv(rd);
  if (sample != NULL)
    printf(">> (key: %s, value: - %s)\n", sample->key, sample->value);
}

void run_sub() {
  dds_entity_t dp = s_create_participant(0);
  dds_entity_t s = s_create_sub(dp);
  // dds_entity_t s = s_create_sub_wp(dp, "alpha");
  dds_entity_t t = s_create_topic_sksv(dp, "KeyValue");

  dds_listener_t* l = dds_listener_create(NULL);
  dds_lset_data_available(l, listener);

  dds_entity_t r = dds_create_reader(s, t, NULL, l);

  while (true) {

      sleep(5);
  }
}

int main(int argc, char* argv[]) {
  if (argc == 1)
    run_pub();
  else
    run_sub();
  return 0;
}
