<script setup lang="ts">
const scenarios = {
  commuteLeisure: { label: "Commute/Leisure Trip", class: '' },
  businessTrip: { label: "Business Trip", class: '' }
};

const alternatives = {
  integratedRoutePlanning: { label: "Integrated Route Planning", class: 'italic text-green-500 dark:text-green-400' },
  multipleRoutePlanners: { label: "Multiple Route Planners", class: 'italic text-yellow-500 dark:text-yellow-400' }
};

const attributes = {
  time: {
    label: { value: "Total Travel Time", class: 'text-purple-500 dark:text-purple-400' },
    levels: {
      twentyMinutes: { value: "20 Minutes", class: 'bg-green-500/50 dark:bg-green-400/50' },
      thirtyFiveMinutes: { value: "35 Minutes", class: 'bg-yellow-500/50 dark:bg-yellow-400/50' },
      fiftyMinutes: { value: "50 Minutes", class: 'bg-red-500/50 dark:bg-red-400/50' }
    }
  },
  experience: {
    label: { value: "Route Planning Experience", class: 'text-orange-500 dark:text-orange-400' },
    levels: {
      veryEasy: { value: "Very Easy", class: 'bg-green-500/50 dark:bg-green-400/50' },
      manageable: { value: "Manageable", class: 'bg-yellow-500/50 dark:bg-yellow-400/50' },
      cumbersome: { value: "Cumbersome", class: 'bg-red-500/50 dark:bg-red-400/50' }
    }
  },
  datashared: {
    label: { value: "Personal data shared between providers", class: 'text-teal-500 dark:text-teal-400' },
    levels: {
      realtime: { value: "Realtime Location Data", class: 'bg-blue-500/50 dark:bg-blue-400/50' },
      anonymized: { value: "Anonymized Data", class: 'bg-yellow-500/50 dark:bg-yellow-400/50' },
      none: { value: "None", class: 'bg-green-500/50 dark:bg-green-400/50' }
    }
  }
};

const columns = [{
  key: 'attribute',
  label: scenarios.commuteLeisure.label, 
  class: scenarios.commuteLeisure.class // Styling from scenarios
}, {
  key: 'alternative0',
  label: alternatives.integratedRoutePlanning.label,
  class: alternatives.integratedRoutePlanning.class // Styling from alternatives
}, {
  key: 'alternative1',
  label: alternatives.multipleRoutePlanners.label,
  class: alternatives.multipleRoutePlanners.class // Styling from alternatives
}]

const items = [{
  attribute: attributes.time.label,
  alternative0: attributes.time.levels.twentyMinutes,
  alternative1: attributes.time.levels.fiftyMinutes,
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
    <template #attribute-data="{ row }">
      <span :class="row.attribute.class">{{ row.attribute.value }}</span>
    </template>
    <template #alternative0-data="{ row }">
      <span :class="row.alternative0.class">{{ row.alternative0.value }}</span>
    </template>
    <template #alternative1-data="{ row }">
      <span :class="row.alternative1.class">{{ row.alternative1.value }}</span>
    </template>
  </UTable>
</template>
