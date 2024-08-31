<script setup lang="ts">
const scenarios = [
  "Commute/Leisure Trip",
  "Business Trip"
];

const alternatives = [
  "Integrated Route Planning",
  "Multiple Route Planners"
];

const attributes = {
  time: {
    label: "Total Travel Time",
    levels: {
      level1: "20 Minutes",
      level2: "35 Minutes",
      level3: "50 Minutes"
    }
  },
  experience: {
    label: "Route Planning Experience",
    levels: {
      veryEasy: "Very Easy",
      manageable: "Manageable",
      cumbersome: "Cumbersome"
    }
  },
  datashared: {
    label: "Personal data shared between providers",
    levels: {
      realtime: "Realtime Location Data",
      anonymized: "Anonymized Data",
      none: "None"
    }
  }
};

const columns = [{
  key: 'attribute',
  label: scenarios[0], // This could be customized per scenario
}, {
  key: 'alternative0',
  label: alternatives[0],
  class: 'italic'
}, {
  key: 'alternative1',
  label: alternatives[1],
}]

const items = [{
  attribute: attributes.time.label,
  alternative0: {value: attributes.time.levels.level1, class: 'bg-green-500/50 dark:bg-green-400/50'},
  alternative1: {value: attributes.time.levels.level3, class: 'bg-red-500/50 dark:bg-red-400/50'},
}, {
  attribute: attributes.experience.label,
  alternative0: {value: attributes.experience.levels.manageable, class: 'bg-yellow-500/50 dark:bg-yellow-400/50'},
  alternative1: {value: attributes.experience.levels.cumbersome, class: 'bg-red-500/50 dark:bg-red-400/50'},
}, {
  attribute: attributes.datashared.label,
  alternative0: {value: attributes.datashared.levels.realtime, class: 'bg-blue-500/50 dark:bg-blue-400/50'},
  alternative1: {value: attributes.datashared.levels.anonymized, class: 'bg-yellow-500/50 dark:bg-yellow-400/50'},
}];
</script>

<template>
  <UTable :rows="items" :columns="columns">
    <template #alternative0-data="{ row }">
      <span :class="row.alternative0.class">{{ row.alternative0.value }}</span>
    </template>
    <template #alternative1-data="{ row }">
      <span :class="row.alternative1.class">{{ row.alternative1.value }}</span>
    </template>
  </UTable>
</template>
