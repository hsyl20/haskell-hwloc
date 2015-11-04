#include <hwloc.h>
#include <stdio.h>

int main() {
   struct hwloc_obj obj;

   printf("type offset %d\n", (char*)&obj.type - (char*)&obj);
   printf("os_index offset %d\n", (char*)&obj.os_index - (char*)&obj);
   printf("name offset %d\n", (char*)&obj.name - (char*)&obj);
   printf("memory offset %d\n", (char*)&obj.memory - (char*)&obj);
   printf("attr offset %d\n", (char*)&obj.attr - (char*)&obj);
   printf("depth offset %d\n", (char*)&obj.depth - (char*)&obj);
   printf("logical_index offset %d\n", (char*)&obj.logical_index - (char*)&obj);
   printf("next_cousin offset %d\n", (char*)&obj.next_cousin - (char*)&obj);
   printf("prev_cousin offset %d\n", (char*)&obj.prev_cousin - (char*)&obj);
   printf("parent offset %d\n", (char*)&obj.parent - (char*)&obj);
   printf("sibling_rank offset %d\n", (char*)&obj.sibling_rank - (char*)&obj);
   printf("next_sibling offset %d\n", (char*)&obj.next_sibling - (char*)&obj);
   printf("prev_sibling offset %d\n", (char*)&obj.prev_sibling - (char*)&obj);
   printf("arity offset %d\n", (char*)&obj.arity - (char*)&obj);
   printf("children offset %d\n", (char*)&obj.children - (char*)&obj);
   printf("first_child offset %d\n", (char*)&obj.first_child - (char*)&obj);
   printf("last_child offset %d\n", (char*)&obj.last_child - (char*)&obj);
   printf("symmetric_subtree offset %d\n", (char*)&obj.symmetric_subtree - (char*)&obj);
   printf("io_arity offset %d\n", (char*)&obj.io_arity - (char*)&obj);
   printf("io_first_child offset %d\n", (char*)&obj.io_first_child - (char*)&obj);
   printf("misc_arity offset %d\n", (char*)&obj.misc_arity - (char*)&obj);
   printf("misc_first_child offset %d\n", (char*)&obj.misc_first_child - (char*)&obj);
   printf("cpuset offset %d\n", (char*)&obj.cpuset - (char*)&obj);
   printf("complete_cpuset offset %d\n", (char*)&obj.complete_cpuset - (char*)&obj);
   printf("allowed_cpuset offset %d\n", (char*)&obj.allowed_cpuset - (char*)&obj);
   printf("nodeset offset %d\n", (char*)&obj.nodeset - (char*)&obj);
   printf("complete_nodeset offset %d\n", (char*)&obj.complete_nodeset - (char*)&obj);
   printf("allowed_nodeset offset %d\n", (char*)&obj.allowed_nodeset - (char*)&obj);
   printf("distances offset %d\n", (char*)&obj.distances - (char*)&obj);
   printf("distances_count offset %d\n", (char*)&obj.distances_count - (char*)&obj);
   printf("infos offset %d\n", (char*)&obj.infos - (char*)&obj);
   printf("infos_count offset %d\n", (char*)&obj.infos_count - (char*)&obj);
   printf("userdata offset %d\n", (char*)&obj.userdata - (char*)&obj);

   printf("sizeof %d\n", sizeof(obj));

   return 0;
}
