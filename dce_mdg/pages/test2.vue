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
    levels: [
      "20 Minutes",
      "35 Minutes",
      "50 Minutes"
    ]
  },
  experience: {
    label: "Route Planning Experience",
    levels: [
      "Very Easy",
      "Manageable",
      "Cumbersome"
    ]
  },
  datashared: {
    label: "Personal data shared between providers",
    levels: [
      "Realtime Location Data",
      "Anonymized Data",
      "None"
    ]
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
  alternative0: {value: attributes.time.levels[0], class: 'bg-green-500/50 dark:bg-green-400/50' },
  alternative1: attributes.time.levels[2],  // "50 Minutes"
}, {
  attribute: attributes.experience.label,
  alternative0: attributes.experience.levels[1],  // "Manageable"
  alternative1: attributes.experience.levels[2],  // "Cumbersome"
}, {
  attribute: attributes.datashared.label,
  alternative0: attributes.datashared.levels[0],  // "Realtime Location Data"
  alternative1: attributes.datashared.levels[1],  // "Anonymized Data"
}];
</script>

<template>
  <UTable :rows="items" :columns="columns">
    <template #alternative0-data="{ row }">
      {{ row.alternative0.value }}
    </template>
  </UTable>
</template>
