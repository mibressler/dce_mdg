<script setup lang="ts">
const scenarios = [
  { label: "Commute/Leisure Trip", class: 'text-blue-500 dark:text-blue-400' },
  { label: "Business Trip", class: 'text-red-500 dark:text-red-400' }
];

const alternatives = [
  { label: "Integrated Route Planning", class: 'italic text-green-500 dark:text-green-400' },
  { label: "Multiple Route Planners", class: 'italic text-yellow-500 dark:text-yellow-400' }
];

const attributes = {
  time: {
    label: "Total Travel Time",
    levels: {
      level1: { value: "20 Minutes", class: 'bg-green-500/50 dark:bg-green-400/50' },
      level2: { value: "35 Minutes", class: 'bg-yellow-500/50 dark:bg-yellow-400/50' },
      level3: { value: "50 Minutes", class: 'bg-red-500/50 dark:bg-red-400/50' }
    }
  },
  experience: {
    label: "Route Planning Experience",
    levels: {
      veryEasy: { value: "Very Easy", class: 'bg-green-500/50 dark:bg-green-400/50' },
      manageable: { value: "Manageable", class: 'bg-blue-500/50 dark:bg-yellow-400/50' },
      cumbersome: { value: "Cumbersome", class: 'bg-red-500/50 dark:bg-red-400/50' }
    }
  },
  datashared: {
    label: "Personal data shared between providers",
    levels: {
      realtime: { value: "Realtime Location Data", class: 'bg-blue-500/50 dark:bg-blue-400/50' },
      anonymized: { value: "Anonymized Data", class: 'bg-blue-500/50 dark:bg-yellow-400/50' },
      none: { value: "None", class: 'bg-green-500/50 dark:bg-green-400/50' }
    }
  }
};

const columns = [{
  key: 'attribute',
  label: scenarios[0].label, 
  class: scenarios[0].class // Added styling from scenarios
}, {
  key: 'alternative0',
  label: alternatives[0].label,
  class: alternatives[0].class // Added styling from alternatives
}, {
  key: 'alternative1',
  label: alternatives[1].label,
  class: alternatives[1].class // Added styling from alternatives
}]

const items = [{
  attribute: attributes.time.label,
  alternative0: attributes.time.levels.level1,
  alternative1: attributes.time.levels.level3,
}, {
  attribute: attributes.experience.label,
  alternative0: attributes.experience.levels.manageable,
  alternative1: attributes.experience.levels.cumbersome,
}, {
  attribute: attributes.datashared.label,
  alternative0: attributes.datashared.levels.realtime,
  alternative1: attributes.datashared.levels.anonymized,
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
