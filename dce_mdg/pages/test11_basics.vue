<script setup lang="ts">
const scenarios = {
  commuteLeisure: { label: "Commute/Leisure Trip", class: ''},
  businessTrip: { label: "Business Trip", class: '' }
};

const alternatives = {
  integratedRoutePlanning: { label: "Integrated Route Planning", class: 'bold bg-gray-100' },
  multipleRoutePlanners: { label: "Multiple Route Planners", class: 'bold bg-gray-100' }
};

const attributes = {
  time: {
    label: { 
      value: "Total Travel Time", 
      class: 'bg-gray-100', 
      icon: 'i-heroicons-clock'
    },
    levels: {
      min20: { value: "20 Minutes", class: 'text-green-600' },
      min35: { value: "35 Minutes", class: 'text-orange-500' },
      min50: { value: "50 Minutes", class: 'text-red-600' }
    }
  },
  experience: {
    label: { 
      value: "Route Planning Experience", 
      class: 'bg-gray-100',
      icon: 'i-heroicons-map'
    },
    levels: {
      veryEasy: { value: "🥰 Very Easy", class: '' },
      manageable: { value: "🙂 Manageable", class: '' },
      cumbersome: { value: "😒 Cumbersome", class: '' }
    }
  },
  datashared: {
    label: { 
      value: "Personal data shared", 
      class: 'bg-gray-100',
      icon: 'i-heroicons-lock-closed'
    },
    levels: {
      realtime: { value: "Realtime Location Data", class: 'font-bold' },
      anonymized: { value: "Anonymized Occupancy Data", class: 'font-bold' },
      none: { value: "Nothing shared", class: 'font-bold' }
    }
  }
};

const columns = [{
  key: 'attribute',
  label: scenarios.commuteLeisure.label, 
  class: scenarios.commuteLeisure.class // Styling from scenarios
}, 
{
  key: 'alternative0',
  label: alternatives.integratedRoutePlanning.label,
  class: alternatives.integratedRoutePlanning.class // Styling from alternatives
},
 {
  key: 'alternative1',
  label: alternatives.multipleRoutePlanners.label,
  class: alternatives.multipleRoutePlanners.class // Styling from alternatives
}]

const items = [{
  attribute: attributes.time.label,
  alternative0: attributes.time.levels.min35,
  alternative1: attributes.time.levels.min35,
}, {
  attribute: attributes.experience.label,
  alternative0: attributes.experience.levels.manageable,
  alternative1: attributes.experience.levels.manageable,
}, {
  attribute: attributes.datashared.label,
  alternative0: attributes.datashared.levels.realtime,
  alternative1: attributes.datashared.levels.none,
}];

const forbidden_level_pairs = [
  // Travel Time
  {alternative0: attributes.time.levels.min20, alternative1: attributes.time.levels.min20},
  {alternative0: attributes.time.levels.min35, alternative1: attributes.time.levels.min20},
  {alternative0: attributes.time.levels.min35, alternative1: attributes.time.levels.min50},
  {alternative0: attributes.time.levels.min50, alternative1: attributes.time.levels.min20},
  {alternative0: attributes.time.levels.min50, alternative1: attributes.time.levels.min35},
  {alternative0: attributes.time.levels.min50, alternative1: attributes.time.levels.min50},

  // Route Planning Experience
  {alternative0: attributes.experience.levels.veryEasy, alternative1: attributes.experience.levels.veryEasy},
  {alternative0: attributes.experience.levels.manageable, alternative1: attributes.experience.levels.veryEasy},
  {alternative0: attributes.experience.levels.manageable, alternative1: attributes.experience.levels.cumbersome},
  {alternative0: attributes.experience.levels.cumbersome, alternative1: attributes.experience.levels.veryEasy},
  {alternative0: attributes.experience.levels.cumbersome, alternative1: attributes.experience.levels.manageable},
  {alternative0: attributes.experience.levels.cumbersome, alternative1: attributes.experience.levels.cumbersome},

  // Data Shared
  {alternative0: attributes.datashared.levels.none, alternative1: attributes.datashared.levels.anonymized},
  {alternative0: attributes.datashared.levels.none, alternative1: attributes.datashared.levels.realtime},
  {alternative0: attributes.datashared.levels.anonymized, alternative1: attributes.datashared.levels.anonymized},
  {alternative0: attributes.datashared.levels.anonymized, alternative1: attributes.datashared.levels.realtime},
  {alternative0: attributes.datashared.levels.realtime, alternative1: attributes.datashared.levels.anonymized},
  {alternative0: attributes.datashared.levels.realtime, alternative1: attributes.datashared.levels.realtime},
];

</script>

<template>
  <UTable :rows="items" :columns="columns">
    <template #attribute-data="{ row }">
      <span :class="row.attribute.class" class="flex items-center">
        <UIcon :name="row.attribute.icon" class="w-5 h-5 mr-2" /> <!-- Displaying the Nuxt UI icon -->
        <span>{{ row.attribute.value }}</span>
      </span>
    </template>
    <template #alternative0-data="{ row }">
      <span :class="row.alternative0.class">{{ row.alternative0.value }}</span>
    </template>
    <template #alternative1-data="{ row }">
      <span :class="row.alternative1.class">{{ row.alternative1.value }}</span>
    </template>
  </UTable>
</template>

<style scoped>

</style>
